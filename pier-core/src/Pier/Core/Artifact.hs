{- | A generic approach to building and caching file outputs.

This is a layer on top of Shake which enables build actions to be written in a
"forwards" style.  For example:

> runPier $ action $ do
>     contents <- lines <$> readArtifactA (external "result.txt")
>     let result = "result.tar"
>     runCommandOutput result
>        $ foldMap input contents
>          <> prog "tar" (["-cf", result] ++ map pathIn contents)

This approach generally leads to simpler logic than backwards-defined build systems such as
make or (normal) Shake, where each step of the build logic must be written as a
new build rule.

Inputs and outputs of a command must be declared up-front, using the 'input'
and 'output' functions respectively.  This enables isolated, deterministic
build steps which are each run in their own temporary directory.

Output files are stored in the location

> _pier/artifact/HASH/path/to/file

where @HASH@ is a string that uniquely determines the action generating
that file.  In particular, there is no need to worry about choosing distinct names
for outputs of different commands.

Note that 'Development.Shake.Forward' has similar motivation to this module,
but instead uses @fsatrace@ to detect what files changed after the fact.
Unfortunately, that approach is not portable.  Additionally, it makes it
difficult to isolate steps and make the build more reproducible (for example,
to prevent the output of one step being mutated by a later one) since every
output file could potentially be an input to every action.  Finally, by
explicitly declaring outputs we can detect sooner when a command doesn't
produce the files that we expect.

-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
module Pier.Core.Artifact
    ( -- * Rules
      artifactRules
    , SharedCache(..)
    , HandleTemps(..)
      -- * Artifact
    , Artifact
    , external
    , (/>)
    , replaceArtifactExtension
    , readArtifact
    , readArtifactB
    , doesArtifactExist
    , matchArtifactGlob
    , unfreezeArtifacts
    , callArtifact
      -- * Creating artifacts
    , writeArtifact
    , runCommand
    , runCommandOutput
    , runCommand_
    , runCommandStdout
    , Command
    , message
      -- ** Command outputs
    , Output
    , output
      -- ** Command inputs
    , input
    , inputs
    , inputList
    , shadow
    , groupFiles
      -- * Running commands
    , prog
    , progA
    , progTemp
    , pathIn
    , withCwd
    , createDirectoryA
    ) where

import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class
import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Simple.Utils (matchDirFileGlob)
import GHC.Generics
import System.Directory as Directory
import System.Exit (ExitCode(..))
import System.Process.Internals (translate)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T hiding (replace)

import Pier.Core.Internal.Directory
import Pier.Core.Internal.HashableSet
import Pier.Core.Internal.Store
import Pier.Core.Persistent

-- | A hermetic build step.  Consists of a sequence of calls to 'message',
-- 'prog'/'progA'/'progTemp', and/or 'shadow', which may be combined using '<>'/'mappend'.
-- Also specifies the input 'Artifacts' that are used by those commands.
data Command = Command
    { _commandProgs :: [Prog]
    , commandInputs :: HashableSet Artifact
    }
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)

data Call
    = CallEnv String -- picked up from $PATH
    | CallArtifact Artifact
    | CallTemp FilePath -- Local file to this Command
                        -- (e.g. generated by an earlier call)
                        -- (This is a hack around shake which tries to resolve
                        -- local files in the env.)
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)

data Prog
    = ProgCall { _progCall :: Call
           , _progArgs :: [String]
           , progCwd :: FilePath  -- relative to the root of the sandbox
           }
    | Message String
    | Shadow Artifact FilePath
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)

instance Monoid Command where
    Command ps is `mappend` Command ps' is' = Command (ps ++ ps') (is <> is')
    mempty = Command [] mempty

instance Semigroup Command where
    (<>) = mappend

-- | Run an external command-line program with the given arguments.
prog :: String -> [String] -> Command
prog p as = Command [ProgCall (CallEnv p) as "."] mempty

-- | Run an artifact as an command-line program with the given arguments.
progA :: Artifact -> [String] -> Command
progA p as = Command [ProgCall (CallArtifact p) as "."]
                $ HashableSet $ Set.singleton p

-- | Run a command-line program with the given arguments, where the program
-- was created by a previous program.
progTemp :: FilePath -> [String] -> Command
progTemp p as = Command [ProgCall (CallTemp p) as "."] mempty

-- | Prints a status message for the user when this command runs.
message :: String -> Command
message s = Command [Message s] mempty

-- | Runs a command within the given (relative) directory.
withCwd :: FilePath -> Command -> Command
withCwd path (Command ps as)
    | isAbsolute path = error $ "withCwd: expected relative path, got " ++ show path
    | otherwise = Command (map setPath ps) as
  where
    setPath m@Message{} = m
    setPath p = p { progCwd = path }

-- | Specify that an 'Artifact' should be made available to program calls within this
-- 'Command'.
--
-- Note that the order does not matter; `input f <> cmd === cmd <> input f`.
input :: Artifact -> Command
input = inputs . Set.singleton

inputList :: [Artifact] -> Command
inputList = inputs . Set.fromList

-- | Specify that a set of 'Artifact's should be made available to program calls within this
-- 'Command'.
inputs :: Set Artifact -> Command
inputs = Command [] . HashableSet

-- | Make a "shadow" copy of the given input artifact's by create a symlink of
-- this artifact (if it is a file) or of each sub-file (transitively, if it is
-- a directory).
--
-- The result may be captured as output, for example when grouping multiple outputs
-- of separate commands into a common directory structure.
shadow :: Artifact -> FilePath -> Command
shadow a f
    | isAbsolute f = error $ "shadowArtifact: need relative destination, found "
                            ++ show f
    | otherwise = Command [Shadow a f] mempty

-- | The output of a given command.
--
-- Multiple outputs may be combined using the 'Applicative' instance.
data Output a = Output [FilePath] (Hash -> a)

instance Functor Output where
    fmap f (Output g h) = Output g (f . h)

instance Applicative Output where
    pure = Output [] . const
    Output f g <*> Output f' g' = Output (f ++ f') (g <*> g')

-- | Register a single output of a command.
--
-- The input must be a relative path and nontrivial (i.e., not @"."@ or @""@).
output :: FilePath -> Output Artifact
output f
    | ds `elem` [[], ["."]] = error $ "can't output empty path " ++ show f
    | ".." `elem` ds  = error $ "output: can't have \"..\" as a path component: "
                                    ++ show f
    | normalise f == "." = error $ "Can't output empty path " ++ show f
    | isAbsolute f = error $ "Can't output absolute path " ++ show f
    | otherwise = Output [f] $ flip builtArtifact f
  where
    ds = splitDirectories f

externalArtifactDir :: FilePath
externalArtifactDir = artifactDir </> "external"

artifactRules :: Maybe SharedCache -> HandleTemps -> Rules ()
artifactRules cache ht = do
    liftIO createExternalLink
    commandRules cache ht
    writeArtifactRules cache
    storeRules

createExternalLink :: IO ()
createExternalLink = do
    exists <- doesPathExist externalArtifactDir
    unless exists $ do
        createParentIfMissing externalArtifactDir
        createDirectoryLink "../.." externalArtifactDir

-- | The build rule type for commands.
data CommandQ = CommandQ
    { commandQCmd :: Command
    , _commandQOutputs :: [FilePath]
    }
    deriving (Eq, Generic)

instance Show CommandQ where
    show CommandQ { commandQCmd = Command progs _ }
        = let msgs = List.intercalate "; " [m | Message m <- progs]
          in "Command" ++
                if null msgs
                    then ""
                    else ": " ++ msgs

instance Hashable CommandQ
instance Binary CommandQ
instance NFData CommandQ

type instance RuleResult CommandQ = Hash

-- TODO: sanity-check filepaths; for example, normalize, should be relative, no
-- "..", etc.
commandHash :: CommandQ -> Action Hash
commandHash cmdQ = do
    let externalFiles = [f | Artifact External f <- Set.toList
                                                        . unHashableSet
                                                        . commandInputs
                                                        $ commandQCmd cmdQ
                           , isRelative f
                        ]
    need externalFiles
    -- TODO: streaming hash
    userFileHashes <- liftIO $ mapM hashExternalFile externalFiles
    makeHash ("commandHash", cmdQ, userFileHashes)

-- | Run the given command, capturing the specified outputs.
runCommand :: Output t -> Command -> Action t
runCommand (Output outs mk) c
    = mk <$> askPersistent (CommandQ c outs)

runCommandOutput :: FilePath -> Command -> Action Artifact
runCommandOutput f = runCommand (output f)

-- Run the given command and record its stdout.
runCommandStdout :: Command -> Action String
runCommandStdout c = do
    out <- runCommandOutput stdoutOutput c
    liftIO $ readFile $ pathIn out

-- | Run the given command without capturing its output.  Can be used to check
-- consistency of the outputs of previous commands.
runCommand_ :: Command -> Action ()
runCommand_ = runCommand (pure ())

commandRules :: Maybe SharedCache -> HandleTemps -> Rules ()
commandRules sharedCache ht = addPersistent $ \cmdQ@(CommandQ (Command progs inps) outs) -> do
    putChatty $ showCommand cmdQ
    h <- commandHash cmdQ
    createArtifacts sharedCache h (progMessages progs) $ \resultDir ->
      -- Run the command within a separate temporary directory.
      -- When it's done, we'll move the explicit set of outputs into
      -- the result location.
      withPierTempDirectoryAction ht (hashString h) $ \tmpDir -> do
        let tmpPathOut = (tmpDir </>)

        liftIO $ collectInputs (unHashableSet inps) tmpDir
        mapM_ (createParentIfMissing . tmpPathOut) outs

        -- Run the command, and write its stdout to a special file.
        root <- liftIO getCurrentDirectory
        stdoutStr <- B.concat <$> mapM (readProg (root </> tmpDir)) progs

        let stdoutPath = tmpPathOut stdoutOutput
        createParentIfMissing stdoutPath
        liftIO $ B.writeFile stdoutPath stdoutStr

        -- Check that all the output files exist, and move them
        -- into the output directory.
        liftIO $ forM_ outs $ \f -> do
            let src = tmpPathOut f
            let dest = resultDir </> f
            exist <- Directory.doesPathExist src
            unless exist $
                error $ "runCommand: missing output "
                        ++ show f
                        ++ " in temporary directory "
                        ++ show tmpDir
            createParentIfMissing dest
            renamePath src dest
    return h

putChatty :: String -> Action ()
putChatty s = do
    v <- shakeVerbosity <$> getShakeOptions
    when (v >= Chatty) $ putNormal s

progMessages :: [Prog] -> [String]
progMessages ps = [m | Message m <- ps]

-- TODO: more hermetic?
collectInputs :: Set Artifact -> FilePath -> IO ()
collectInputs inps tmp = do
    let inps' = dedupArtifacts inps
    checkAllDistinctPaths inps'
    liftIO $ mapM_ (linkArtifact tmp) inps'

-- Call a process inside the given directory and capture its stdout.
-- TODO: more flexibility around the env vars
-- Also: limit valid parameters for the *prog* binary (rather than taking it
-- from the PATH that the `pier` executable sees).
readProg :: FilePath -> Prog -> Action B.ByteString
readProg _ (Message s) = do
    putNormal s
    return B.empty
readProg dir (ProgCall p as cwd) = readProgCall dir p as cwd
readProg dir (Shadow a0 f0) = do
    liftIO $ linkShadow dir a0 f0
    return B.empty

readProgCall :: FilePath -> Call -> [String] -> FilePath -> Action BC.ByteString
readProgCall dir p as cwd = do
    -- hack around shake weirdness w.r.t. relative binary paths
    let p' = case p of
                CallEnv s -> s
                CallArtifact f -> dir </> pathIn f
                CallTemp f -> dir </> f
    (ret, Stdout out, Stderr err)
        <- quietly $ command
                    [ Cwd $ dir </> cwd
                    , Env defaultEnv
                    -- stderr will get printed if there's an error.
                    , EchoStderr False
                    ]
                    p' (map (spliceTempDir dir) as)
    let errStr = T.unpack . T.decodeUtf8With T.lenientDecode $ err
    case ret of
        ExitSuccess -> return out
        ExitFailure ec -> do
            v <- shakeVerbosity <$> getShakeOptions
            fail $ if v < Loud
                -- TODO: remove trailing newline
                then errStr
                else unlines
                        [ showProg (ProgCall p as cwd)
                        , "Working dir: " ++ translate (dir </> cwd)
                        , "Exit code: " ++ show ec
                        , "Stderr:"
                        , errStr
                        ]

-- TODO: use forFileRecursive_
linkShadow :: FilePath -> Artifact -> FilePath -> IO ()
linkShadow dir a0 f0 = do
    createParentIfMissing (dir </> f0)
    loop a0 f0
  where
    loop a f = do
        let aPath = pathIn a
        isDir <- Directory.doesDirectoryExist aPath
        if isDir
            then do
                Directory.createDirectoryIfMissing False (dir </> f)
                cs <- getRegularContents aPath
                mapM_ (\c -> loop (a /> c) (f </> c)) cs
            else do
                srcExists <- Directory.doesFileExist aPath
                destExists <- Directory.doesPathExist (dir </> f)
                let aPath' = case a of
                                Artifact External aa -> "external" </> aa
                                Artifact (Built h) aa -> hashString h </> aa
                if
                    | not srcExists -> error $ "linkShadow: missing source "
                                                ++ show aPath
                    | destExists -> error $ "linkShadow: destination already exists: "
                                                ++ show f
                    | otherwise -> createFileLink
                                    (relPathUp f </> "../../artifact" </> aPath')
                                    (dir </> f)
    relPathUp = joinPath . map (const "..") . splitDirectories . parentDirectory

showProg :: Prog -> String
showProg (Shadow a f) = unwords ["Shadow:", pathIn a, "=>", f]
showProg (Message m) = "Message: " ++ show m
showProg (ProgCall call args cwd) =
    wrapCwd
        . List.intercalate " \\\n    "
        $ showCall call : args
  where
    wrapCwd s = case cwd of
                    "." -> s
                    _ -> "(cd " ++ translate cwd ++ " &&\n " ++ s ++ ")"

    showCall (CallArtifact a) = pathIn a
    showCall (CallEnv f) = f
    showCall (CallTemp f) = f -- TODO: differentiate from CallEnv

showCommand :: CommandQ -> String
showCommand (CommandQ (Command progs inps) outputs) = unlines $
    map showOutput outputs
    ++ map showInput (Set.toList $ unHashableSet inps)
    ++ map showProg progs
  where
    showOutput a = "Output: " ++ a
    showInput i = "Input: " ++ pathIn i

stdoutOutput :: FilePath
stdoutOutput = "_stdout"

defaultEnv :: [(String, String)]
defaultEnv =
    [ ("PATH", "/usr/bin:/bin")
    -- Set LANG to enable TemplateHaskell code reading UTF-8 files correctly.
    , ("LANG", "en_US.UTF-8")
    ]

spliceTempDir :: FilePath -> String -> String
spliceTempDir tmp = T.unpack . T.replace (T.pack "${TMPDIR}") (T.pack tmp) . T.pack

checkAllDistinctPaths :: Monad m => [Artifact] -> m ()
checkAllDistinctPaths as =
    case Map.keys . Map.filter (> 1) . Map.fromListWith (+)
            . map (\a -> (pathIn a, 1 :: Integer)) $ as of
        [] -> return ()
        -- TODO: nicer error, telling where they came from:
        fs -> error $ "Artifacts generated from more than one command: " ++ show fs

-- Remove duplicate artifacts that are both outputs of the same command, and where
-- one is a subdirectory of the other (for example, constructed via `/>`).
dedupArtifacts :: Set Artifact -> [Artifact]
dedupArtifacts = loop . Set.toAscList
  where
    -- Loop over artifacts built from the same command.
    -- toAscList plus lexicographic sorting means that
    -- subdirectories with the same hash will appear consecutively after directories
    -- that contain them.
    loop (a@(Artifact (Built h) f) : Artifact (Built h') f' : fs)
        -- TODO BUG: "Picture", "Picture.hs" and Picture/Foo.hs" sort in the wrong way
        -- so "Picture" and "Picture/Foo.hs" aren't deduped.
        | h == h', (f <//> "*") ?== f' = loop (a:fs)
    loop (f:fs) = f : loop fs
    loop [] = []

-- Symlink the artifact into the given destination directory.
linkArtifact :: FilePath -> Artifact -> IO ()
linkArtifact _ (Artifact External f)
    | isAbsolute f = return ()
linkArtifact dir a = do
    curDir <- getCurrentDirectory
    let realPath = curDir </> realPathIn a
    let localPath = dir </> pathIn a
    createParentIfMissing localPath
    isFile <- Directory.doesFileExist realPath
    if isFile
        then createFileLink realPath localPath
        else do
            isDir <- Directory.doesDirectoryExist realPath
            if isDir
                then createDirectoryLink realPath localPath
                else error $ "linkArtifact: source does not exist: " ++ show realPath
                        ++ " for artifact " ++ show a


-- | Returns the relative path to an Artifact within the sandbox, when provided
-- to a 'Command' by 'input'.
pathIn :: Artifact -> FilePath
pathIn (Artifact External f) = externalArtifactDir </> f
pathIn (Artifact (Built h) f) = hashDir h </> f

-- | Returns the relative path to an artifact within the root directory.
realPathIn :: Artifact -> FilePath
realPathIn (Artifact External f) = f
realPathIn (Artifact (Built h) f) = hashDir h </> f


-- | Replace the extension of an Artifact.  In particular,
--
-- > pathIn (replaceArtifactExtension f ext) == replaceExtension (pathIn f) ext@
replaceArtifactExtension :: Artifact -> String -> Artifact
replaceArtifactExtension (Artifact s f) ext
    = Artifact s $ replaceExtension f ext

-- | Read the contents of an Artifact.
readArtifact :: Artifact -> Action String
readArtifact (Artifact External f) = readFile' f -- includes need
readArtifact f = liftIO $ readFile $ pathIn f

readArtifactB :: Artifact -> Action B.ByteString
readArtifactB (Artifact External f) = need [f] >> liftIO (B.readFile f)
readArtifactB f = liftIO $ B.readFile $ pathIn f

data WriteArtifactQ = WriteArtifactQ
    { writePath :: FilePath
    , writeContents :: String
    }
    deriving (Eq, Typeable, Generic, Hashable, Binary, NFData)

instance Show WriteArtifactQ where
    show w = "Write " ++ writePath w

type instance RuleResult WriteArtifactQ = Artifact

writeArtifact :: FilePath -> String -> Action Artifact
writeArtifact path contents = askPersistent $ WriteArtifactQ path contents

writeArtifactRules :: Maybe SharedCache -> Rules ()
writeArtifactRules sharedCache = addPersistent
        $ \WriteArtifactQ {writePath = path, writeContents = contents} -> do
    h <- makeHash . T.encodeUtf8 . T.pack
                $ "writeArtifact: " ++ contents
    createArtifacts sharedCache h [] $ \tmpDir -> do
        let out = tmpDir </> path
        createParentIfMissing out
        liftIO $ writeFile out contents
    return $ builtArtifact h path

doesArtifactExist :: Artifact -> Action Bool
doesArtifactExist (Artifact External f) = Development.Shake.doesFileExist f
doesArtifactExist f = liftIO $ Directory.doesFileExist (pathIn f)

-- Note: this throws an exception if there's no match.
matchArtifactGlob :: Artifact -> FilePath -> Action [FilePath]
-- TODO: match the behavior of Cabal
matchArtifactGlob (Artifact External f) g
    = getDirectoryFiles f [g]
matchArtifactGlob a g
    = liftIO $ matchDirFileGlob (pathIn a) g

-- TODO: merge more with above code?  How hermetic should it be?
callArtifact :: HandleTemps -> Set Artifact -> Artifact -> [String] -> IO ()
callArtifact ht inps bin args = withPierTempDirectory ht "exec" $ \tmp -> do
    dir <- getCurrentDirectory
    collectInputs (Set.insert bin inps) tmp
    cmd_ [Cwd tmp]
        (dir </> tmp </> pathIn bin) args

createDirectoryA :: FilePath -> Command
createDirectoryA f = prog "mkdir" ["-p", f]

-- | Group source files by shadowing into a single directory.
groupFiles :: Artifact -> [(FilePath, FilePath)] -> Action Artifact
groupFiles dir files = let out = "group"
                   in runCommandOutput out
                        $ createDirectoryA out
                        <> foldMap (\(f, g) -> shadow (dir /> f) (out </> g))
                            files

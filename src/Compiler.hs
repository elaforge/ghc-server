{-# LANGUAGE ScopedTypeVariables #-}
{- | Run a persistent compiler.  It uses the flags passed on startup, ignoring
    any files on the cmdline.  After that it accepts complete cmdlines on
    stdin.  If the flags are are the same as the original startup flags, the
    file is extracted and compiled.  Otherwise, it crashes.
-}
module Compiler where
import Control.Applicative ((<$>))
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.List as List
import qualified Data.Time as Time
import qualified DriverPhases
import qualified DriverPipeline
import qualified GHC
import qualified GHC.Paths
-- The liftIO here is not the same one in Control.Monad.Trans!
-- GHC defines its own MonadIO.
import MonadUtils (MonadIO, liftIO)
import qualified System.Environment
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Util


main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    (_, origFlags) <- parseArgs <$> System.Environment.getArgs
    Util.notice $ "starting up with flags: " ++ unwords origFlags
    (args, staticFlagWarnings) <- GHC.parseStaticFlags
        (map (GHC.mkGeneralLocated "cmdline") origFlags)
    let (origSFlags, _) = splitStaticFlags origFlags
    GHC.runGhc (Just GHC.Paths.libdir) $ do
        origDFlags <- GHC.getSessionDynFlags
        (dflags, args, dynFlagWarnings) <-
            GHC.parseDynamicFlags origDFlags args
        let warns = map GHC.unLoc $ staticFlagWarnings ++ dynFlagWarnings
        when (not (null warns)) $
            throw $ "warnings parsing flags: " ++ show warns
        when (not (null args)) $
            notice $ "ignoring unparsed args: " ++ show (map GHC.unLoc args)
        GHC.setSessionDynFlags dflags
        debug "set flags, loop"
        Util.whileJust (liftIO (Util.ignoreEOF getLine)) $
            processLine origSFlags origDFlags
    Util.notice "exiting"
    where
    processLine origSFlags origDFlags line = do
        args <- liftIO $ Util.unserializeArgs line
        let (requestSources, requestFlags) = parseArgs args
        debug $ "received request: "
            ++ show (requestSources, requestFlags)
        b <- case requestSources of
            [] -> throw $ "no filename in request: " ++ show args
            [fn] -> do
                notice $ "request " ++ show fn
                (b, secs) <- elapsed $
                    compile origSFlags origDFlags requestFlags fn
                notice $ "complete " ++ show fn ++ " " ++ show b ++ ": "
                    ++ show secs
                return b
            _ -> throw $ "multiple files to compile, one at a time please: "
                ++ show requestSources
        liftIO $ putStrLn $ if b then Util.compilerSuccess
            else Util.compilerFailure

-- | Split source files to compile from the flags.
--
-- It's not quite right because it just looks for *.hs and *.lhs, and will
-- thus catch flag arguments.  Getting filtering out flag arguments requires
-- parsing the cmdline as ghc does, but ghc's static flags only export
-- a function that mutates globals and can only be called once.
parseArgs :: [String] -> ([String], [FilePath])
parseArgs = List.partition ((`elem` [".hs", ".lhs"]) . FilePath.takeExtension)
    -- -c is handled by the frontend, and is neither a static or dynamic flag,
    -- but I do '-c' implicitly already so I can ignore it.  The rest of the
    -- front-end flags are in ghc/Main.hs
    . filter (/= "-c")

splitStaticFlags :: [String] -> ([String], [String])
splitStaticFlags = List.partition (`elem` flags)
    where
    flags = map ('-':)
        [ "prof", "eventlog", "parallel", "gransim", "debug", "ndb"
        , "threaded", "ticky"
        , "static", "dynamic", "rdynamic"

        -- Actually there are tons of these -f* flags.
        , "fhpc"
        ]

makeTarget :: FilePath -> GHC.Target
makeTarget filename = GHC.Target
    { GHC.targetId = GHC.TargetFile filename Nothing
    , GHC.targetAllowObjCode = True
    , GHC.targetContents = Nothing
    }

targetFile :: GHC.Target -> Maybe FilePath
targetFile target = case GHC.targetId target of
    GHC.TargetFile filename _ -> Just filename
    _ -> Nothing

compile :: (GHC.GhcMonad m) => [String] -> GHC.DynFlags -> Util.Args -> String
    -> m Bool
compile origSFlags dflags args filename = do
    (sFlags, args) <- return $ splitStaticFlags args
    (dflags, args, warns) <- GHC.parseDynamicFlags dflags $
        map (GHC.mkGeneralLocated "request") args
    -- If I don't set NoLink compileFile ignores -o.
    GHC.setSessionDynFlags $ dflags
        { GHC.ghcLink = GHC.NoLink
        , GHC.ghcMode = GHC.OneShot
        -- GHC.verbosity = 2
        }
    when (not (null warns)) $
        notice $ "warnings parsing dynamic flags: "
            ++ show (map GHC.unLoc warns)
    -- TODO the distributor should start another compiler with these flags
    when (origSFlags /= sFlags) $
        throw $ "static flags " ++ unwords sFlags
            ++ " /= orig flags " ++ unwords origSFlags
    when (not (null args)) $
        notice $ "trailing args: " ++ show (map GHC.unLoc args)
    compileFile filename


-- | Take source file to obj file.
compileFile :: (GHC.GhcMonad m) => FilePath -> m Bool
compileFile file = do
    hscEnv <- GHC.getSession
    liftIO $ (compile hscEnv)
        `Exception.catch` \(exc :: Exception.SomeException) -> do
            Util.error $ "compiler error: " ++ cleanup (show exc)
            return False
    where
    compile hscEnv = do
        DriverPipeline.compileFile hscEnv DriverPhases.StopLn (file, Nothing)
        return True
    cleanup = unlines . filter (not.null) . lines


debug :: (MonadIO m) => String -> m ()
debug = liftIO . Util.debug

notice :: (MonadIO m) => String -> m ()
notice = liftIO . Util.notice

throw :: (MonadIO m) => String -> m a
throw = liftIO . Exception.throwIO . Exception.ErrorCall

elapsed :: (MonadIO m) => m a -> m (a, Time.NominalDiffTime)
elapsed op = do
    start <- liftIO $ Time.getCurrentTime
    r <- op
    end <- liftIO $ Time.getCurrentTime
    return (r, end `Time.diffUTCTime` start)

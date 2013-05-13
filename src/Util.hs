module Util where
import Prelude hiding (log, error)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.Maybe as Maybe
import qualified Network
import qualified Network.Socket as Socket
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process


distributorSocketName :: FilePath
distributorSocketName = "distributor-socket"

compilerFailure, compilerSuccess :: String
compilerFailure = "***failure"
compilerSuccess = "***success"

listen :: FilePath -> IO Socket.Socket
listen socketName = do
    -- Apparently unix sockets are not released when even if I sClose them,
    -- and the next program to bind will get "Address already in use".
    -- I don't think there's a race here.
    ignoreENOENT (Directory.removeFile socketName)
    Network.listenOn (Network.UnixSocket socketName)

connect :: FilePath -> IO (Maybe IO.Handle)
connect name = do
    hdl <- ignoreENOENT $
        Network.connectTo "localhost" (Network.UnixSocket name)
    maybe (return ()) (\h -> IO.hSetBuffering h IO.LineBuffering) hdl
    return hdl

type Args = [String]

serializeArgs :: Args -> String
serializeArgs = show

unserializeArgs :: String -> IO Args
unserializeArgs = readIO

whileJust :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whileJust get f = loop
    where
    loop = do
        val <- get
        case val of
            Nothing -> return ()
            Just v -> f v >> loop

-- | If @op@ raised ENOENT, return Nothing.
ignoreENOENT :: IO a -> IO (Maybe a)
ignoreENOENT op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)

ignoreEOF :: IO a -> IO (Maybe a)
ignoreEOF op = Exception.handleJust (guard . IO.Error.isEOFError)
    (const (return Nothing)) (fmap Just op)

delay :: Double -> IO ()
delay = Concurrent.threadDelay . floor . (*10**6)

-- -- Local connection so the firewall doesn't freak out.
-- findPort :: IO Network.Socket
-- findPort = try 8000
--     where
--     try port = do
--         socket <- Network.listenOn (Network.PortNumber port)

loggedProcess :: Process.CreateProcess -> IO (Maybe IO.Handle,
       Maybe IO.Handle, Maybe IO.Handle, Process.ProcessHandle)
loggedProcess create = do
    r@(_, _, _, pid) <- Process.createProcess create
    Concurrent.forkIO $ do
        code <- Process.waitForProcess pid
        case code of
            Exit.ExitFailure c -> notice $
                "subprocess " ++ show (binaryOf create) ++ " failed: "
                ++ if c == 127 then "binary not found" else show c
            _ -> return ()
    return r
    where
    binaryOf create = case Process.cmdspec create of
        Process.RawCommand fn _ -> fn
        Process.ShellCommand cmd -> fst $ break (==' ') cmd


-- * logging

verbosity :: String
verbosity = "GHC_SERVER_VERBOSITY"

getEnv :: String -> IO (Maybe String)
getEnv var = do
    env <- Environment.getEnvironment
    return $ lookup var env

debug :: String -> IO ()
debug msg = log 3 msg

notice :: String -> IO ()
notice msg = log 2 msg

error :: String -> IO ()
error msg = log 1 msg

log :: Int -> String -> IO ()
log level msg = do
    v <- fmap (Maybe.fromMaybe "2") $ getEnv verbosity
    when (show level <= v) $ do
        prog <- Environment.getProgName
        putStrLn $ prog ++ ": " ++ msg

{-# LANGUAGE ScopedTypeVariables #-}
{- | Listen on the socket.  Each connection is dispatched to a worker

    FE detects distributor isn't running, starts one with its flags.

    FE submits a cmdline.

    An unbusy compiler managed by the distributor grabs the cmdline and
    compiles it.

    Only capture stdout, leave stderr.  Pass through stdout lines as they come
    in, until '***[01]', this is the success.

    Return success to FE, which returns itself.

    If the distributors compilers are all idle for a certain amount of time,
    it shuts down on its own.


    TODO:
    The FE passes the whole arg list every time, and the distributor has to be
    parse it and extract the source file.

    Is -o considered a static flag?
-}
module Distributor where
import Control.Applicative ((<$>))
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Network
import qualified System.Environment
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Util


-- Shutdown automatically after a while if there are no requests.
-- TODO Or, if there is a request with different argv, kill and restart the
-- compilers.
--
-- If multiple distributors start simultaneously, they'll fight over the
-- socket.  Hopefully one will get it and the rest will die when they try to
-- listen on it.

cpus :: Int
cpus = 4

-- | Idle this long before shutting down.
--
-- On shutdown, set 'shutdown' so working threads can shutdown on their own.
-- Kill the thread waiting on Network.accept.
idleSeconds :: Double
idleSeconds = 10

compilerBinary :: String
compilerBinary = "ghc-compiler"

main :: IO ()
main = Network.withSocketsDo $ do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    args <- System.Environment.getArgs
    Util.notice $ "starting up: " ++ unwords args
    feSocket <- Util.listen Util.distributorSocketName
    chan <- Chan.newChan
    shutdown <- IORef.newIORef False
    lastAccess <- IORef.newIORef =<< Time.getCurrentTime
    busy <- IORef.newIORef Map.empty
    mapM_ (Concurrent.forkIO . worker shutdown busy chan args) [0..cpus-1]
    listenerId <- Concurrent.forkIO $ listener lastAccess chan feSocket
    watchdog lastAccess shutdown listenerId feSocket

watchdog :: IORef.IORef Time.UTCTime -> IORef.IORef Bool
    -> Concurrent.ThreadId -> Network.Socket -> IO ()
watchdog lastAccess shutdown listenerId feSocket = loop
    where
    loop = do
        Util.delay 0.25
        last <- IORef.readIORef lastAccess
        now <- Time.getCurrentTime
        if (realToFrac (Time.diffUTCTime now last) >= idleSeconds)
            then do
                Util.debug "no requests for a while, shutting down"
                IORef.writeIORef shutdown True
                Concurrent.killThread listenerId
                Network.sClose feSocket
            else loop

listener :: IORef.IORef Time.UTCTime -> Chan.Chan Message -> Network.Socket
    -> IO ()
listener lastAccess chan feSocket = forever $ do
    (handle, _, _) <- Network.accept feSocket
    IORef.writeIORef lastAccess =<< Time.getCurrentTime
    Concurrent.forkIO $ cleanup handle $ do
        eof <- IO.hIsEOF handle
        if eof then Util.notice "unexpected EOF from frontend" else do
        request <- Util.unserializeArgs =<< IO.hGetLine handle
        Util.debug $ "received request: " ++ show request
        mvar <- MVar.newEmptyMVar
        Chan.writeChan chan (Message request mvar)
        success <- MVar.takeMVar mvar
        IO.hPutStrLn handle $ if success
            then Util.compilerSuccess else Util.compilerFailure
    where
    cleanup handle op = Exception.finally op (IO.hClose handle)
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Util.error $ "listener died: " ++ show exc

-- | Map of which workers are doing work.
type Busy = IORef.IORef (Map.Map WorkerId Bool)

worker :: IORef.IORef Bool
    -- ^ True if the distributor wants to shut down.
    -> Busy -> Chan.Chan Message -> Util.Args -> WorkerId -> IO ()
worker shutdown busy chan args wid = do
    msg <- Chan.readChan chan
    ghc <- startGhc busy wid args
    go ghc msg
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Util.error $ "worker " ++ show wid ++ " died: " ++ show exc
    where
    go ghc (Message args mvar) = do
        IORef.atomicModifyIORef busy $ \m -> (Map.insert wid True m, ())
        success <- compile ghc args
        IORef.atomicModifyIORef busy $ \m -> (Map.insert wid False m, ())
        MVar.putMVar mvar success
        Util.debug $ "compile complete: " ++ show success
        continue <- not <$> IORef.readIORef shutdown
        when (continue && success) $
            go ghc =<< Chan.readChan chan

compile :: Ghc -> Util.Args -> IO Bool
compile ghc args = do
    Concurrent.forkIO $ do
        IO.hPutStrLn (ghcStdin ghc) (Util.serializeArgs args)
        IO.hFlush (ghcStdin ghc)
        Util.debug $ "send to compiler: " ++ show args
    Util.debug $ "compile: " ++ show args
    go
    where
    go = do
        mbLine <- Chan.readChan (ghcStdout ghc)
        case mbLine of
            Nothing -> Util.notice "EOF from compiler" >> return False
            Just line
                | line == Util.compilerFailure -> return False
                | line == Util.compilerSuccess -> return True
                | otherwise -> putStrLn line >> go

startGhc :: Busy -> WorkerId -> Util.Args -> IO Ghc
startGhc busy wid args = do
    Util.debug $ "start ghc: " ++ show args
    (Just stdin, Just out, _stderr, pid) <- Util.loggedProcess
        (Process.proc compilerBinary args)
            { Process.std_in = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            }
    chan <- Chan.newChan
    Concurrent.forkIO $ void $ Util.ignoreEOF $
        forever (Chan.writeChan chan . Just =<< addPrefix =<< IO.hGetLine out)
            `Exception.finally` Chan.writeChan chan Nothing
    return $ Ghc stdin chan pid
    where
    addPrefix line
        | line `elem` [Util.compilerFailure, Util.compilerSuccess] =
            return line
        | otherwise = do
            count <- length . filter id . Map.elems <$> IORef.readIORef busy
            let idstr = show count ++ " (" ++ show wid ++ ")"
            return $ if null post
                then idstr ++ ": " ++ pre
                else pre ++ " " ++ idstr ++ post
        where (pre, post) = break (==':') line

-- | A number to identify each worker.  It's passed to the workers themselves
-- so they can include it in output.
type WorkerId = Int

data Ghc = Ghc {
    ghcStdin :: IO.Handle
    -- | Lines from the subprocess, or a Nothing on EOF.
    , ghcStdout :: Chan.Chan (Maybe String)
    , ghcPid :: Process.ProcessHandle
    }

-- | File to compile, success response.
data Message = Message Util.Args (MVar.MVar Bool)

module FE where
import Control.Monad
import qualified Network
import qualified System.Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Util


distributorBinary :: FilePath
distributorBinary = "ghc-distributor"

-- | Wait this long for the distributor to get its act together and start
-- listening on the socket.
startupDelay :: Double
startupDelay = 0.5

main :: IO ()
main = Network.withSocketsDo $ do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    args <- System.Environment.getArgs
    maybe_hdl <- Util.connect Util.distributorSocketName
    hdl <- case maybe_hdl of
        Nothing -> do
            startDistributor args
            Util.delay startupDelay
            maybe_hdl <- Util.connect Util.distributorSocketName
            maybe (error $ "can't connect to distributor even after "
                ++ "trying to start one") return maybe_hdl
        Just hdl -> return hdl
    Util.debug $ "send: " ++ (Util.serializeArgs args)
    IO.hPutStrLn hdl (Util.serializeArgs args)
    response <- Util.ignoreEOF $ IO.hGetLine hdl
    Util.debug $ "response: " ++ (show response)
    case response of
        Nothing -> error $ "unexpected EOF from distributor"
        Just r
            | r == Util.compilerFailure -> Exit.exitFailure
            | r == Util.compilerSuccess -> Exit.exitSuccess
            | otherwise -> error $
                "unexpected response from distributor: " ++ show response

startDistributor :: [String] -> IO ()
startDistributor args = do
    Util.notice $ "no one listening, starting a distributor"
    void $ Util.loggedProcess (Process.proc distributorBinary args)

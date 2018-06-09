{-# LANGUAGE MagicHash, TypeFamilies, DataKinds,
             FlexibleContexts, TypeOperators,
             MultiParamTypeClasses #-}
module Network.Wai.Servlet.Handler.Jetty
   ( Options(), defaultOptions, runJetty, run ) where
import qualified Network.Wai          as Wai     
import           Network.Wai.Servlet
import           Data.Maybe ( fromMaybe, isJust)
import           Control.Monad ( when )
import           Java

data {-# CLASS "org.eclipse.jetty.util.component.LifeCycle" #-}
  LifeCycle = LifeCycle (Object# LifeCycle)
  deriving Class

foreign import java unsafe "@interface" start ::
  (a <: LifeCycle) => Java a ()

foreign import java unsafe "@interface" stop ::
  (a <: LifeCycle) => Java a ()

data {-# CLASS "org.eclipse.jetty.server.Handler" #-}
  Handler = Handler (Object# Handler) deriving Class
  
data {-# CLASS "org.eclipse.jetty.server.handler.AbstractHandler" #-}
  AbstractHandler = AbstractHandler (Object# AbstractHandler)
  deriving Class

type instance Inherits AbstractHandler = '[Object,Handler]

data {-# CLASS "org.eclipse.jetty.server.Request" #-}
  Request = Request (Object# Request) 
  deriving Class

foreign import java unsafe setHandled :: Bool -> Java Request ()

foreign import java unsafe "@wrapper @abstract handle"
   wrapHandler :: ( JString -> Request ->
                    HttpServletRequest -> HttpServletResponse ->
                    Java AbstractHandler () ) -> AbstractHandler

makeHandler :: Wai.Application -> AbstractHandler
makeHandler waiApp = wrapHandler h
   where h _ req servReq servResp = do
           io $ waiApp waiReq waiRespond
           req <.> (setHandled True)
              where waiReq = makeWaiRequest servReq
                    waiRespond = updateHttpServletResponse
                      servReq servResp  

data {-# CLASS "org.eclipse.jetty.server.Connector" #-}
  Connector = Connector (Object# Connector)
  deriving Class

type instance Inherits Connector = '[LifeCycle]

data {-# CLASS "org.eclipse.jetty.server.AbstractConnector" #-}
  AbstractConnector = AbstractConnector (Object# AbstractConnector)
  deriving Class

type instance Inherits AbstractConnector = '[Object,Connector]

data {-# CLASS "org.eclipse.jetty.server.AbstractNetworkConnector" #-}
  AbstractNetworkConnector = AbstractNetworkConnector
     (Object# AbstractNetworkConnector) deriving Class

type instance Inherits AbstractNetworkConnector = '[AbstractConnector]

data {-# CLASS "org.eclipse.jetty.server.ServerConnector" #-}
  ServerConnector = ServerConnector (Object# ServerConnector)
  deriving Class

type instance Inherits ServerConnector = '[AbstractNetworkConnector]

foreign import java unsafe "@new" serverConnector ::
  Server -> ConnectionFactoryArray -> ServerConnector

foreign import java unsafe setPort ::
   (a <: AbstractNetworkConnector) => Int -> Java a ()

foreign import java unsafe setHost ::
   (a <: AbstractNetworkConnector) => Maybe String -> Java a ()

foreign import java unsafe setIdleTimeout ::
   (a <: AbstractConnector) => Int64 -> Java a ()

data {-# CLASS "org.eclipse.jetty.server.HandlerWrapper" #-}
  HandlerWrapper = HandlerWrapper (Object# HandlerWrapper)
  deriving Class

type instance Inherits HandlerWrapper = '[Object,LifeCycle]

foreign import java unsafe setHandler ::
  (a <: Handler , b <: HandlerWrapper) => a -> Java b () 

data {-# CLASS "org.eclipse.jetty.server.Server" #-}
  Server = Server (Object# Server)
  deriving Class

type instance Inherits Server = '[HandlerWrapper]

foreign import java unsafe "@new" server :: Server

foreign import java unsafe addConnector ::
  (a <: Connector) => a -> Java Server ()


data {-# CLASS "org.eclipse.jetty.server.ConnectionFactory" #-}
  ConnectionFactory = ConnectionFactory (Object# ConnectionFactory)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.HttpConnectionFactory" #-}
  HttpConnectionFactory = HttpConnectionFactory
     (Object# HttpConnectionFactory)
  deriving Class

type instance Inherits HttpConnectionFactory = '[ConnectionFactory]

foreign import java unsafe "@new" httpConnectionFactory ::
   HttpConfiguration -> HttpConnectionFactory

data {-# CLASS "org.eclipse.jetty.server.ConnectionFactory[]" #-}
  ConnectionFactoryArray = ConnectionFactoryArray
     (Object# ConnectionFactoryArray) deriving Class

instance JArray ConnectionFactory ConnectionFactoryArray

data {-# CLASS "org.eclipse.jetty.server.HttpConfiguration" #-}
  HttpConfiguration = HttpConfiguration (Object# HttpConfiguration)
  deriving Class

foreign import java unsafe "@new" httpConfiguration ::
   HttpConfiguration

data Options = Options { host :: Maybe String , port :: Int
                       , idleTimeout :: Int }

defaultOptions = Options { host = Nothing , port = 3000
                         , idleTimeout = 200000 }

httpConfig :: Options -> HttpConfiguration
httpConfig opts = httpConfiguration

doto :: (Class c) => c -> [Java c a] -> Java b [a]
doto c = withObject c . sequence 

httpConnector :: Server -> Options -> Java a ServerConnector
httpConnector serv opts = do
  let httpFact = httpConnectionFactory $ httpConfig opts
  httpFactArr <- arrayFromList [superCast httpFact]
  let servConn = serverConnector serv httpFactArr
  withObject servConn $ do
    setPort $ port opts
    setIdleTimeout $ fromIntegral $ idleTimeout opts
    let host'=host opts
    when (isJust host') $ setHost host'
    -- Error if we pass Nothing directly to setHost
  return servConn

createServer :: Options -> Java a Server
createServer opts = do
  let s = server
  conn <- httpConnector s opts
  s <.> (addConnector conn)
  return s

runJetty :: Options -> Wai.Application -> Java a Server
runJetty opts app = do
  serv <- createServer opts
  doto serv [setHandler $ makeHandler app, start]
  return serv

run :: Int -> Wai.Application -> IO ()
run port' app = do
  let opts = defaultOptions { port = port' }
  java $ runJetty opts app
  return ()
  

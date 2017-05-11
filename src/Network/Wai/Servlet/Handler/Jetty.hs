{-# LANGUAGE MagicHash, TypeFamilies, DataKinds,
             FlexibleContexts, TypeOperators,
             MultiParamTypeClasses #-}
module Network.Wai.Servlet.Handler.Jetty where
import qualified Network.Wai          as Wai     
import           Network.Wai.Servlet  as JWai
import           Java

data {-# CLASS "org.eclipse.jetty.server.handler.AbstractHandler" #-}
  AbstractHandler = AbstractHandler (Object# AbstractHandler)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.Request" #-}
  Request = Request (Object# Request) 
  deriving Class

foreign import java unsafe "@wrapper @abstract handle"
  abstractHandler :: (Request -> ServletRequest -> ServletResponse ->
                      Java AbstractHandler ()) -> AbstractHandler 

data {-# CLASS "org.eclipse.jetty.server.ServerConnector" #-}
  ServerConnector = ServerConnector (Object# ServerConnector)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.Server" #-}
  Server = Server (Object# Server)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.ConnectionFactory" #-}
  ConnectionFactory = ConnectionFactory (Object# ConnectionFactory)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.ConnectionFactory[]" #-}
  ConnectionFactoryArray = ConnectionFactoryArray
     (Object# ConnectionFactoryArray) deriving Class

instance JArray ConnectionFactory ConnectionFactoryArray

foreign import java unsafe "@new" serverConnector ::
  Server -> ConnectionFactoryArray -> ServerConnector

data {-# CLASS "org.eclipse.jetty.server.HttpConfiguration" #-}
  HttpConfiguration = HttpConfiguration (Object# HttpConfiguration)
  deriving Class

foreign import java unsafe "@new" httpConfiguration :: HttpConfiguration

data Options = Options 

httpConfig :: Options -> HttpConfiguration
httpConfig opts = undefined

httpConnector :: Server -> Options -> ServerConnector
httpConnector serv opts = undefined



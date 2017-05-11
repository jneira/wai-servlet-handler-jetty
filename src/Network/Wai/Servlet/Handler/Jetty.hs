{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,
             FlexibleContexts,TypeOperators #-}
module Network.Wai.Servlet.Handler.Jetty where
import Network.Wai               
import Network.Wai.Servlet.Request
import Network.Wai.Servlet.Response
import Java

data {-# CLASS org.eclipse.jetty.server.handler.AbstractHandler -}
   AbstractHandler = AbstractHandler (Object# AbstractHandler)
data {-# CLASS org.eclipse.jetty.server.Request -}
   Request = Request (Object# Request) 

foreign import java unsafe "@wrapper @abstract handle"
   abstractHandler :: Request -> ServletRequest -> ServletResponse ->
                      AbstractHandler 

--makeAbstract



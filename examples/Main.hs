import Network.Wai.Servlet.Handler.Jetty
import Network.Wai.Servlet.Examples

main = run 3000 $ appFile "examples/index.html"

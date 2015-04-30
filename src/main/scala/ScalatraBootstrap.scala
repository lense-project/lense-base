import javax.servlet.ServletContext

import edu.stanford.lense_base.server.{WebsocketsServlet, ScalatraTest}
import org.scalatra.LifeCycle
import org.scalatra.servlet.RichServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context : ServletContext) {
    // Mount servlets.
    context.mount(new WebsocketsServlet, "/*")
  }
}

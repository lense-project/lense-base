import javax.servlet.ServletContext

import edu.stanford.lense_base.server.WorkUnitServlet
import org.atmosphere.cpr.ApplicationConfig
import org.scalatra.LifeCycle
import org.scalatra.servlet.RichServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context : ServletContext) {
    // Mount servlets.
    context.setInitParameter(ApplicationConfig.SCAN_CLASSPATH, "false")
    context.setInitParameter("org.atmosphere.cpr.CometSupport.maxInactiveActivity", "1000")
    context.setInitParameter("org.atmosphere.websocket.maxIdleTime", "1000")
    context.mount(WorkUnitServlet, "/*")
  }
}

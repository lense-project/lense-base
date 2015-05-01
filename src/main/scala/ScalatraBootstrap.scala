import javax.servlet.ServletContext

import edu.stanford.lense_base.server.WorkUnitServlet
import org.atmosphere.cpr.ApplicationConfig
import org.atmosphere.interceptor.IdleResourceInterceptor
import org.scalatra.LifeCycle
import org.scalatra.servlet.RichServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context : ServletContext) {
    // Mount servlets.
    context.setInitParameter(ApplicationConfig.SCAN_CLASSPATH, "false")
    context.mount(WorkUnitServlet, "/*")
  }
}

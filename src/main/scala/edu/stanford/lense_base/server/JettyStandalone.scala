package edu.stanford.lense_base.server

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server._
import org.eclipse.jetty.server.ssl.SslSocketConnector
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.eclipse.jetty.webapp.WebAppContext

import org.eclipse.jetty.util.ssl.SslContextFactory

/**
 * Created by keenon on 5/15/15.
 *
 * Creates a Jetty Standalone server, with an SSL layer, so it can talk to Turkers
 */
class JettyStandalone(webapp : String) {

  // the keystore (with one key) we'll use to make the connection with the
  // broker
  private val KEYSTORE_LOCATION = "src/main/resources/jetty/keystore"
  private val KEYSTORE_PASS = "passwd"

  val server = new Server()
  val connector = new SelectChannelConnector()
  connector.setPort(8080)
  server.addConnector(connector)

  val context: WebAppContext = new WebAppContext(webapp, "/")
  context.setServer(server)
  server.setHandler(context)

  val sslContextFactory = new SslContextFactory(KEYSTORE_LOCATION)
  sslContextFactory.setKeyStorePassword(KEYSTORE_PASS)
  sslContextFactory.setNeedClientAuth(false)

  // create a https connector
  val sslConnector = new SslSocketConnector(sslContextFactory)
  sslConnector.setPort(8443)
  server.addConnector(sslConnector)

  try {
    server.start()
  } catch {
    case e: Exception => {
      e.printStackTrace()
      System.exit(1)
    }
  }
}

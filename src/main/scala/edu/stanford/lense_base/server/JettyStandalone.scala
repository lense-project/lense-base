package edu.stanford.lense_base.server

import java.io.File

import org.apache.commons.logging.impl.NoOpLog
import org.apache.log4j.Level
import org.apache.log4j.spi.NOPLogger
import org.eclipse.jetty.server._
import org.eclipse.jetty.webapp.WebAppContext

/*

// Jetty 8

import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.ssl.SslSocketConnector
import org.eclipse.jetty.servlet.ServletContextHandler
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.eclipse.jetty.webapp.WebAppContext

import org.eclipse.jetty.util.ssl.SslContextFactory
*/

// Jetty 9

import org.eclipse.jetty.http.HttpVersion
import org.eclipse.jetty.security.HashLoginService
import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.HttpConfiguration
import org.eclipse.jetty.server.HttpConnectionFactory
import org.eclipse.jetty.server.LowResourceMonitor
import org.eclipse.jetty.server.NCSARequestLog
import org.eclipse.jetty.server.SecureRequestCustomizer
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.ServerConnector
import org.eclipse.jetty.server.SslConnectionFactory
import org.eclipse.jetty.server.handler.ContextHandlerCollection
import org.eclipse.jetty.server.handler.DefaultHandler
import org.eclipse.jetty.server.handler.HandlerCollection
import org.eclipse.jetty.server.handler.RequestLogHandler
import org.eclipse.jetty.server.handler.StatisticsHandler
import org.eclipse.jetty.util.ssl.SslContextFactory
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.eclipse.jetty.util.thread.ScheduledExecutorScheduler

import org.eclipse.jetty.util.log.Logger
import org.eclipse.jetty.util.log.StdErrLog

/**
 * Created by keenon on 5/15/15.
 *
 * Creates a Jetty Standalone server, with an SSL layer, so it can talk to Turkers
 */
class JettyStandalone(webapp : String) {

  // the keystore (with one key) we'll use to make the connection with the
  // broker
  private val KEYSTORE_LOCATION = "/etc/apache2/ssl/keystore"
  private val KEYSTORE_PASS = "passwd"

  /*

  // Jetty 8

  val server = new Server()

  val context: WebAppContext = new WebAppContext(webapp, "/")
  context.setServer(server)
  context.setInitParameter("cacheControl", "max-age=0,public")
  context.setInitParameter("org.atmosphere.cpr.broadcasterCacheClass", "org.atmosphere.cache.UUIDBroadcasterCache")
  server.setHandler(context)

  val sslContextFactory = new SslContextFactory(KEYSTORE_LOCATION)
  sslContextFactory.setKeyStorePassword(KEYSTORE_PASS)
  sslContextFactory.setNeedClientAuth(false)

  // create a https connector
  val sslConnector = new SslSocketConnector(sslContextFactory)
  sslConnector.setPort(8443)
  server.addConnector(sslConnector)

  val connector = new SelectChannelConnector()
  connector.setPort(8080)
  server.addConnector(connector)

  */

  // Setup Threadpool
  val threadPool = new QueuedThreadPool()
  threadPool.setMaxThreads(500)

  val server = new Server(threadPool)

  val useDevPorts = false

  val publicPort = if (useDevPorts) 8081 else 8080
  val securePort = if (useDevPorts) 8444 else 8443

  val http_config = new HttpConfiguration()
  http_config.setSecureScheme("https")
  http_config.setSecurePort(securePort)
  http_config.setOutputBufferSize(32768)
  http_config.setRequestHeaderSize(8192)
  http_config.setResponseHeaderSize(8192)
  http_config.setSendServerVersion(true)
  http_config.setSendDateHeader(false)

  val http = new ServerConnector(server,
    new HttpConnectionFactory(http_config))
  http.setPort(publicPort) // 8080
  http.setIdleTimeout(30000)
  server.addConnector(http)

  if (new File(KEYSTORE_LOCATION).exists()) {
    val sslContextFactory = new SslContextFactory()
    sslContextFactory.setKeyStorePath(KEYSTORE_LOCATION)
    sslContextFactory.setKeyStorePassword(KEYSTORE_PASS)
    sslContextFactory.setExcludeCipherSuites("SSL_RSA_WITH_DES_CBC_SHA",
      "SSL_DHE_RSA_WITH_DES_CBC_SHA", "SSL_DHE_DSS_WITH_DES_CBC_SHA",
      "SSL_RSA_EXPORT_WITH_RC4_40_MD5",
      "SSL_RSA_EXPORT_WITH_DES40_CBC_SHA",
      "SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA",
      "SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA")

    // SSL HTTP Configuration
    val https_config = new HttpConfiguration(http_config)
    https_config.addCustomizer(new SecureRequestCustomizer())

    // SSL Connector
    val sslConnector = new ServerConnector(server,
      new SslConnectionFactory(sslContextFactory,HttpVersion.HTTP_1_1.asString()),
      new HttpConnectionFactory(https_config))
    sslConnector.setPort(securePort)
    server.addConnector(sslConnector)
  }

  val context: WebAppContext = new WebAppContext(webapp, "/")
  context.setServer(server)
  context.setInitParameter("cacheControl", "max-age=0,public")
  context.setInitParameter("org.atmosphere.cpr.broadcasterCacheClass", "org.atmosphere.cache.UUIDBroadcasterCache")
  server.setHandler(context)

  org.eclipse.jetty.util.log.Log.setLog(new StdErrLog())
  /*
  // Disable Jetty logging
  org.eclipse.jetty.util.log.Log.setLog(new Logger {
    override def getName: String = "No logs"
    override def warn(msg: String, args: AnyRef*): Unit = System.err.println("WARN: "+msg)
    override def warn(thrown: Throwable): Unit = thrown.printStackTrace()
    override def warn(msg: String, thrown: Throwable): Unit = {
      System.err.println("WARN: "+msg)
      thrown.printStackTrace()
    }
    override def isDebugEnabled: Boolean = false
    override def getLogger(name: String): Logger = this
    override def ignore(ignored: Throwable): Unit = {}
    override def debug(msg: String, args: AnyRef*): Unit = {}
    override def debug(msg: String, value: Long): Unit = {}
    override def debug(thrown: Throwable): Unit = {}
    override def debug(msg: String, thrown: Throwable): Unit = {}
    override def setDebugEnabled(enabled: Boolean): Unit = {}
    override def info(msg: String, args: AnyRef*): Unit = {}
    override def info(thrown: Throwable): Unit = {}
    override def info(msg: String, thrown: Throwable): Unit = {}
  })
  */

  try {
    server.start()
  } catch {
    case e: Exception =>
      e.printStackTrace()
      System.exit(1)
  }
}

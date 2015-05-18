name := "lense-base"

version := "1.0"

scalaVersion := "2.11.4"

resolvers += "Akka Repo" at "http://repo.akka.io/repository"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.10"

libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "2.1.8"

// libraryDependencies += "org.eclipse.jetty.aggregate" % "jetty-all-server" % "9.2.10.v20150310" // "8.1.16.v20140903"

libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.2.10.v20150310"

libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.2.10.v20150310"

libraryDependencies += "org.eclipse.jetty.websocket" % "websocket-server" % "9.2.10.v20150310" // "8.1.16.v20140903"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.3.0.RC1" // RC1

libraryDependencies += "org.scalatra" %% "scalatra" % "2.4.0.RC1"

libraryDependencies += "org.atmosphere" % "atmosphere-runtime" % "2.2.0"

libraryDependencies += "org.scalatra" %% "scalatra-atmosphere" % "2.4.0.RC1"

libraryDependencies += "org.scalatra" %% "scalatra-json" % "2.2.0"

libraryDependencies += "org.scalatra" %% "scalatra-scalate" % "2.4.0.RC1"

libraryDependencies += "org.scalatra" %% "scalatra-specs2" % "2.4.0.RC1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime"

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0"

libraryDependencies += "com.esotericsoftware.kryo" % "kryo" % "2.24.0"

// This seems to be necessary for specs2
resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

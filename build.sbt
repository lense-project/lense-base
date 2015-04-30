name := "lense-base"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "com.googlecode.lanterna" % "lanterna" % "2.1.8"

libraryDependencies += "org.eclipse.jetty.aggregate" % "jetty-all-server" % "8.1.16.v20140903"

// libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.1.5.v20140505"

// libraryDependencies += "org.eclipse.jetty" % "jetty-webapp" % "9.1.5.v20140505"

// libraryDependencies += "org.eclipse.jetty" % "jetty-plus" % "9.1.5.v20140505"

libraryDependencies += "org.scalatra" %% "scalatra" % "2.4.0.RC1"

libraryDependencies += "org.scalatra" %% "scalatra-scalate" % "2.4.0.RC1"

libraryDependencies += "org.scalatra" %% "scalatra-specs2" % "2.4.0.RC1" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2" % "runtime"

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0"

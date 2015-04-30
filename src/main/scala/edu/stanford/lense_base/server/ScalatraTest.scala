package edu.stanford.lense_base.server

import org.scalatra._

class ScalatraTest extends ScalatraServlet {

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
      </body>
    </html>
  }
}
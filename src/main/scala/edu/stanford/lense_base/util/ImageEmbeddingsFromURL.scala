package edu.stanford.lense_base.util

import scala.util.matching.Regex
import sys.process._
import java.net.URL
import java.io.File

/**
 * Created by keenon on 5/26/15.
 *
 * Runs against Caffe. Basically a bunch of system calls, some parsing, and voilla
 */
object ImageEmbeddingsFromURL {
  def embeddingsForURL(url : String) : List[Double] = {
    val name = url.split("/").last
    val download = "curl "+url+" > /tmp/"+name
    println("Running: "+download)
    (new URL(url) #> new File("/tmp/"+name)).!!
    val getEmbeddings = "src/main/python/run_embedding.sh /tmp/"+name
    println("Running: "+getEmbeddings)
    val output = getEmbeddings.!!.split("\\s").toList
    output.slice(1, output.size).filter{
      case d : String => d.length() > 0
    }.map(d => java.lang.Double.parseDouble(d.replaceAll("]","").trim()))
  }

  def main (args: Array[String]) {
    println(embeddingsForURL("http://1.bp.blogspot.com/_Y7rzCyUABeI/SNIltEyEnjI/AAAAAAAABOg/E1keU_52aFc/s400/ash_abhishek_365x470.jpg"))
  }
}

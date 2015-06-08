package edu.stanford.mydo

import edu.stanford.nlp.annotation.AnnotationClient
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.{SemanticGraphCoreAnnotations, SemanticGraph}
import edu.stanford.nlp.semgraph.semgrex.{SemgrexMatcher, SemgrexPattern}
import edu.stanford.nlp.util.CoreMap

import scala.collection.mutable.ListBuffer

import scala.collection.JavaConversions._

/**
 * Created by keenon on 6/8/15.
 *
 * A stupid little case class to hold all the emails we care to handle, along with useful annotations
 */
case class Email(body : String, title : String, to : List[String], from : String, id : BigInt) {
  val annotation: Annotation = AnnotationClient.annotate(body)
  val sentences: List[CoreMap] = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).toList

  val todos: ListBuffer[String] = ListBuffer[String]()
}

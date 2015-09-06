package edu.stanford.mydo

import java.util
import java.util.Arrays

import edu.stanford.nlp.annotation.AnnotationClient
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.{SemanticGraphCoreAnnotations, SemanticGraph}
import edu.stanford.nlp.semgraph.semgrex.{SemgrexMatcher, SemgrexPattern}
import edu.stanford.nlp.util.CoreMap
import scala.concurrent.{Await, Promise}
import scala.io.Source
import scala.util.{Random, Try}
import scala.concurrent.duration._

import scala.collection.mutable.ListBuffer

/**
 * Created by keenon on 6/8/15.
 *
 * A system that first uses regex, then confirms intuitions using LENSE, then does deterministic syntactic transforms
 */
object TodoExtractor {

  val emailUseCase = {
    val u = new EmailUseCase()
    u.ensureWorkServer
    u.initialize()
    u
  }

  val flags = Set(
    "could you",
    "would you",
    "would you mind",
    "it would be awesome if",
    "you should",
    "I need you to",
    "Don't forget to"
  )

  def extractTodos(email : Email) : List[String] = {
    for (sentence <- email.sentences) {
      val dependencies: SemanticGraph = sentence.get(classOf[SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation])
      val p: SemgrexPattern = SemgrexPattern.compile("({}=head >aux {}=could >nsubj {lemma:you}=you)")
      val m: SemgrexMatcher = p.matcher(dependencies)

      var mayContainTodos = true
      /*
      while (m.find) {
        val word: IndexedWord = m.getNode("head")
        mayContainTodos = true
      }

      for (flag <- flags) {
        if (sentence.toString.contains(flag)) {
          mayContainTodos = true
        }
      }
      */

      if (mayContainTodos) {
        // email.todos.+=(clipSentence(sentence, word.index - 1))
        val toInclude = Await.result(emailUseCase.getOutput((email, email.sentences.indexOf(sentence))).future, 1000 days) == "YES"
        if (toInclude) {
          email.todos.+=(sentence.toString)
        }
      }

      import scala.collection.JavaConversions._
      for (token <- sentence.get(classOf[CoreAnnotations.TokensAnnotation])) {
        val word: String = token.get(classOf[CoreAnnotations.TextAnnotation])
        val pos: String = token.get(classOf[CoreAnnotations.PartOfSpeechAnnotation])
        val ne: String = token.get(classOf[CoreAnnotations.NamedEntityTagAnnotation])
      }
    }

    email.todos.toList
  }

  private def clipSentence(sentence: CoreMap, fromIndex: Int): String = {
    var fromI : Int = fromIndex
    val sb: StringBuilder = new StringBuilder
    import scala.collection.JavaConversions._
    for (token <- sentence.get(classOf[CoreAnnotations.TokensAnnotation])) {
      val pos: String = token.get(classOf[CoreAnnotations.PartOfSpeechAnnotation])
      if (!Arrays.asList("?", ".", ":", ";", ",").contains(pos)) {
        if (fromI < 0) sb.append(" ")
        if ({
          fromI -= 1
          fromI + 1
        } <= 0) {
          sb.append(token.word)
        }
      }
    }
    sb.toString()
  }
}

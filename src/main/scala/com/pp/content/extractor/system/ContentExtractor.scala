package com.pp.content.extractor.system

import akka.actor.{ActorLogging, Actor, Props}
import com.pp.content.extractor.system.BodyGetter.BadStatus
import org.jsoup.Jsoup

import scala.concurrent.ExecutionContext
import scala.util.matching.Regex
import akka.pattern.pipe

/**
 * A role of ContentExtractor actor is to get proper content from website. Actor gets a body from provided website and
 * extracts content based on `ExtractionTask`. Furthermore, actor search for URL's on the website and send it with
 * content to its parent.
 */
object ContentExtractor{
  import scala.collection.JavaConversions._

  /**
   * For creation purposes
   * params correspond with [[com.pp.content.extractor.system.ContentExtractor]] class
   */
  def props(url: String, urlRegex: Regex, task: ExtractionTask, depth: Int) =
    Props(new ContentExtractor(url, urlRegex,task, depth))

  /**
   * Helper method that parses body using `Jsoup` and performs searching for content as well as links on this site
   * @param body - to parse
   * @param urlRegex - only URL's that matches this pattern will be send to parent
   * @param extractionTask - extraction description
   * @return - tuple of contents and links
   */
  def extract(body: String, urlRegex: Regex, extractionTask: ExtractionTask): (List[String], List[String]) = {
    implicit val document = Jsoup.parse(body)
    val contents =
      (for {
        extraction <- extractionTask.extractions
        content <- extraction.extract
      } yield content).toList

    val links = (
      for {
        cont <- document.select("a[href]")
        attr = cont.attr("href")
        if urlRegex.findAllMatchIn(attr).hasNext
      } yield cont.attr("href")).toList
    (contents, links)
  }

}

/**
 * Actor that execute searching
 * @param depth - it is used to keep actual depth rate and send it back to parent
 */
class ContentExtractor(url: String, urlRegex: Regex, extractionTask: ExtractionTask, depth: Int) extends Actor with ActorLogging {

  implicit val exec: ExecutionContext = context.dispatcher

  log.info(s"Searching for content in: $url")

  /** send body to itself */
  BodyGetter.getBody(url).pipeTo(self)

  def receive = {
    case body: String =>
      val (contents, links) = ContentExtractor.extract(body, urlRegex, extractionTask)
      stopJob(Complete(contents, links, depth))
    case BadStatus(status) =>
      stopJob(Error(s"Bad status: $status", url))
    case err: Throwable =>
      stopJob(Error(err.getMessage, url))
  }

  def stopJob(message: Any) = {
    context.parent ! message
    context.stop(self)
  }

}
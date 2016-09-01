package com.pp.content.extractor.system

import akka.actor.{ActorRef, ActorLogging, Actor}
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

/**
 * Turning message to the client with final result of searching
 * @param content - result
 * @param visited - url's that were visited
 */
case class Result(content: List[String], visited: Set[String])

/**
 * Turning message to the `ExtractorManager` with partial result from single task
 * @param content - partial result
 * @param links - url's that were found on url
 * @param depth - actual depth of task
 */
case class Complete(content: List[String], links: List[String], depth: Int)

/**
 * Turning message to the `ExtractorManager` that flag an error
 * @param cause - cause of failure
 * @param link - link that cause failure
 */
case class Error(cause: String, link: String)

/**
 * Start message to the `ExtractorManager`
 */
case object Start

/**
 * Stop message to the `ExtractorManager`
 */
case object Stop

/**
 * Manages content searching. It protects from double visiting, and too deep searching
 * @param baseUrl - start url
 * @param matchedUrl - it's passed to `ContentExtractors` to filter url's
 * @param task - extraction description
 * @param depth - max depth of search
 */
class ExtractorManager(baseUrl: java.net.URL, matchedUrl: Regex, task: ExtractionTask, depth: Int) extends Actor with ActorLogging {

  /** to create unique children names */
  var workerNum = 0

  /** set of visited url's */
  var visited = Set[String]()
  var content = List[String]()

  /** it's use to check if we can stop job*/
  var children = Set[ActorRef]()

  self ! Start

  override def receive = {
    case Complete(cont, links, dep) =>
      /**
       * Helper method that corrects incomplete url's - e.g. changes `/sample` to http://www.sample.com/sample`
       */
      def correctUrl(url: String): Try[String] = {
        Try{new java.net.URL(url)} match {
          case Success(u) => Success(u.toString)
          case Failure(e) => Try{new java.net.URL(baseUrl, url).toString}
        }
      }

      content = content ++ cont
      children -= sender

      /** checks if we mustn't go deeper*/
      if (dep > 0) {
        links.foreach { url =>
          correctUrl(url) match {
            case Success(link) if !visited(link) => /** checks if we've already been there*/
              visited += link
              children += context.actorOf(
                ContentExtractor.props(link, matchedUrl, task, dep - 1),
                name = s"ContentExtractor$workerNum")
              workerNum += 1

            case Success(link) =>
              log.info(s"Already visited: $link")

            case Failure(e) =>
              log.info(s"Error with : $url ,message: $e")
          }
        }
      }
      /** checks if we can stop the job*/
      if(children.size == 0){
        self ! Stop
      }

    case Error(cause, link) =>
      log.info(s"Error with : $link ,message: $cause")

    /** starts with baseUrl */
    case Start =>
      children += context.actorOf(
        ContentExtractor.props(baseUrl.toString, matchedUrl, task, depth),
        name = s"ContentExtractor$workerNum")
      visited += baseUrl.toString
      workerNum += 1

    case Stop =>
      context.parent ! Result(content, visited)
      context.stop(self)
  }

}


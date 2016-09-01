package com.pp.content.extractor.system

import play.api.libs.ws.WS

import scala.concurrent.{Promise, Future, ExecutionContext}
import scala.util.{Failure, Success}

/**
 * Object provides a way to get content from websites
 */
object BodyGetter {
  // initialization of `NingWSClient`
  private val builder = new com.ning.http.client.AsyncHttpClientConfig.Builder()
  implicit private val client = new play.api.libs.ws.ning.NingWSClient(builder.build())

  /**
   * class that represents BadStatus exception
   * @param status - status code
   */
  case class BadStatus(status: Int) extends RuntimeException

  /**
   * gets content from provided url
   * @param url -
   * @param exec - to `Future` execution
   */
  def getBody(url: String)(implicit exec: ExecutionContext): Future[String] = {
    val p = Promise[String]()
    WS.clientUrl(url).get.onComplete{
      case Success(resp) =>
        if (resp.status < 400)
          p.success(resp.body)
        else p.failure(BadStatus(resp.status))
      case Failure(e) => p.failure(e)
    }
    p.future
  }

  /**
   * Client should be closed
   */
  def close() = client.close
}

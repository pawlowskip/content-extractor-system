package com.pp.content.extractor.system

import java.nio.file.Paths

import akka.actor._

import scala.xml.XML

/**
  * gets content from http://www.ang.pl/slownictwo/phrasal-verbs and extracts phrasal verbs with translations and
  * examples to html file phrasal1.html
  */
class Vocabulary extends Actor {

  /**
    * generates html with vocabulary
    * @param docTitle title of documents
    * @param separator separator between columns
    * @param docContent rows with content
    * @return generated html
    */
  def generateHtml(docTitle: String, separator: String, docContent: List[String]): String = {
    import scalatags.Text.all._

    def extract(string: String): Array[String] = string.split(separator)

    def row(string: String) =
      tr(
        extract(string).map( str => td(str) )
      )

    val createTable =
      table(
        docContent.map(row(_))
      )

    "<!DOCTYPE html>" +
    html(
      head(
        meta(charset := "utf-8")
      ),
      body(
        h1(docTitle),
        createTable
      )
    ).render
  }

  val sep = "    "

  val extractor = context.actorOf(
    Props(
      new ExtractorManager(new java.net.URL("http://www.ang.pl/slownictwo/phrasal-verbs/zawez/3"),
        """/slownictwo/phrasal-verbs/zawez/3.*""".r,
        ExtractionTask(
          ExtractionSet("div.row[style=border-bottom: 1px solid #ccc;]",
            Text("div.medium-4 p"),
            Text("div.medium-3 p"),
            Text("div.medium-5 p")
          ){(seq: Seq[String]) => seq.mkString(sep)}
        ),
        depth = 2)
    ),
    name = "extractor"
  )

  def receive = {
    case Result(content, visited) =>

      val sorted = content.sorted

      println(sorted.mkString("Content: \n", "\n", "\n-----------------------\n"))
      println(visited.mkString("Content: \n", "\n", "\n-----------------------\n"))

      val htmlText = generateHtml("Phrasal Verbs - obowiązkowe", sep, sorted)
      val p = new scala.xml.PrettyPrinter(1000, 4)

      val pretty = p.format(XML.loadString(htmlText))

      import java.nio.charset.StandardCharsets

      java.nio.file.Files.write(Paths.get("phrasal1.html"), pretty.getBytes(StandardCharsets.UTF_8))

      context.stop(self)
  }

  override def postStop = {
    BodyGetter.close()
  }

}
package com.pp.content.extractor.system

import java.nio.file.Paths

import akka.actor._

import scala.xml.XML

class Main extends Actor {

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

    html(
      head(),
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


class Job extends Actor {
  val extractor = context.actorOf(
    Props(
      new ExtractorManager(new java.net.URL("http://pl.indeed.com/praca?q=Java&l="),
        """(/praca?q=Scala&start=.*)|(/rc/clk?.*)""".r,
        ExtractionTask(
            Text("body")
        ),
        depth = 5)
    ),
    name = "job"
  )

  override def receive = {
    case Result(content, visited) =>
      println(s"Visited: ${visited.size}")
      println(visited.mkString("\n"))
      var master = 0
      var beachelor = 0
      var degree = 0

      for (c <- content.map(_.toLowerCase)) {
        println(c)
        val general = c.indexOf("degree") > 0 || c.indexOf("wykształcenie") > 0 || c.indexOf("studia") > 0
        val bech = c.indexOf("bachelor") > 0 || c.indexOf("inżyniersk") > 0 || c.indexOf("licencjat") > 0
        val mast = c.indexOf("master") > 0 || c.indexOf("magister") > 0
        if (general) degree += 1
        if (general && bech) beachelor += 1
        if (general && mast) master += 1
      }

      println(s"degree: $degree")
      println(s"bachelor: $beachelor")
      println(s"master: $master")
      context.stop(self)
  }
}
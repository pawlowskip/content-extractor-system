package com.pp.content.extractor.system

import org.jsoup.nodes.Element


trait Extraction {
  def extract(implicit element: Element): Seq[String]
}

case class Text(selection: String) extends Extraction {
  import scala.collection.JavaConversions._
  def extract(implicit element: Element) =
    for (cont: Element <- element.select(selection)) yield cont.text
}

case class Attr(selection: String, name: String) extends Extraction {
  import scala.collection.JavaConversions._
  def extract(implicit element: Element) =
    for (cont: Element <- element.select(selection)) yield cont.attr(name)
}

case class ExtractionSet(selection: String, extractions: Extraction*)(bind: Seq[String] => String ) extends Extraction {

  import scala.collection.JavaConversions._
  def extract(implicit element: Element) =
    for {
      cont: Element <- element.select(selection)
    } yield bind(extractions.flatMap(_.extract(cont)))

}

case class ExtractionTask(extractions: Extraction*)

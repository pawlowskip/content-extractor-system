name := "content-extractor-system"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  ws,
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.14",
  "org.jsoup" % "jsoup" % "1.8.3",
  "com.lihaoyi" %% "scalatags" % "0.5.2"
)
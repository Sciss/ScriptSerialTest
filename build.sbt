scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

fork in run := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

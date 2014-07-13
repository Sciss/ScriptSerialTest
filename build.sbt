scalaVersion := "2.11.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

fork in run := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

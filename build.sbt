ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(name := "project-graph-query")
  .aggregate(`dot-parser`, `csv-parser`, `graph-enricher`)

lazy val `dot-parser` = (project in file("dot-parser"))
  .settings(
    name := "dot-parser",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.8",
      "co.fs2"        %% "fs2-io"      % "3.6.1"
    )
  )

lazy val `csv-parser` = (project in file("csv-parser"))
  .settings(
    name := "csv-parser",
    libraryDependencies ++= Seq(
      "org.typelevel"        %% "cats-effect"          % "3.4.8",
      "co.fs2"               %% "fs2-io"               % "3.6.1",
      "org.gnieh"            %% "fs2-data-csv"         % "1.7.1",
      "org.gnieh"            %% "fs2-data-csv-generic" % "1.7.1",
      "com.github.tototoshi" %% "scala-csv"            % "1.3.10"
    )
  )

lazy val `graph-enricher` = (project in file("graph-enricher"))
  .settings(
    name := "graph-enricher",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"  % "3.4.8",
      "co.fs2"        %% "fs2-io"       % "3.6.1"
    )
  ).dependsOn(`dot-parser`, `csv-parser`)

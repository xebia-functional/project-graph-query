package com.xebia.functional.graph.dot

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*

import java.io.InputStreamReader

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    DOTParser
      .impl[IO]
      .parseDOT(
        new InputStreamReader(getClass.getResourceAsStream("/projects-graph.dot"))
      )
      .flatMap { diGraph =>
        IO.println(s"""
             |Read DiGraph successfully
             |
             |Nodes
             |${diGraph.nodes.map(_.show).mkString("\n")}
             |
             |Relations
             |${diGraph.relations.map(_.show).mkString("\n")}
             |""".stripMargin)
      }
      .as(ExitCode.Success)

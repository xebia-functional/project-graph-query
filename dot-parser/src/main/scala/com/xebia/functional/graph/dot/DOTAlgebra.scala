package com.xebia.functional.graph.dot

import cats.effect.{Async, Resource}
import cats.syntax.all.*
import fs2.Stream

import java.io.{BufferedReader, BufferedWriter, FileReader, Reader, Writer}

trait DOTAlgebra[F[_]]:
  def parseDOT(reader: Reader): F[DiGraph]
  def writeDOT(writer: Writer, diGraph: DiGraph): F[Unit]

object DOTAlgebra:

  private val NodeRegEx = "^\"(.+)\"\\[.+]$".r
  private val NodeRelationRegEx = "^\"(.+)\" -> \"(.+)\" \\[(.+)]$".r
  private val KeyValueRegEx = "(.+)=\"?(.+)\"?".r

  def impl[F[_]: Async]: DOTAlgebra[F] = new DOTAlgebra[F]:

    private def readLine(br: BufferedReader): F[Option[(String, BufferedReader)]] =
      Async[F].delay(Option(br.readLine()).map((_, br)))

    private def parseAttributes(attr: String): Map[String, String] =
      attr
        // TODO - Values between quotes could contains ','
        .split(',').toList.flatMap {
          case KeyValueRegEx(key, value) =>
            List(key -> value)
          case _ => Nil
        }.toMap

    private def parse(line: String): Option[Either[Node, NodeRelation]] =
      line.trim match
        case NodeRegEx(name) => Node(name, none).asLeft.some
        case NodeRelationRegEx(n1, n2, attr) =>
          NodeRelation(n1, n2, parseAttributes(attr)).asRight.some
        case _ => none

    override def parseDOT(reader: Reader): F[DiGraph] =
      Stream
        .fromAutoCloseable(Async[F].delay(new BufferedReader(reader)))
        .flatMap(Stream.unfoldEval(_)(readLine))
        .fold(DiGraph(Nil, Nil)) { (diGraph, line) =>
          parse(line).map(_.fold[DiGraph](diGraph.:+, diGraph.:+)).getOrElse(diGraph)
        }
        .compile
        .lastOrError

    private def printAttributes(map: Map[String, String]): String =
      map.toList.filter(_._2.nonEmpty).map((k, v) => s"\"$k\"=\"$v\"").mkString(",")

    private def printShape(node: Node): String =
      node.shape.map(",shape=" + _).getOrElse("")

    override def writeDOT(writer: Writer, diGraph: DiGraph): F[Unit] =
      Resource.fromAutoCloseable(Async[F].delay(new BufferedWriter(writer))).use { writer =>
        Async[F].delay(
          writer.write("""digraph "projects-graph" {
            |    graph[rankdir="LR"]
            |    node [
            |        shape="record"
            |    ]
            |    edge [
            |        arrowtail="none"
            |    ]
            |""".stripMargin)
        ) >> diGraph.nodes.traverse_(node => Async[F].delay(writer.write(s"""   "${node.name}"[label=<${node.name}>${printShape(node)}]\n"""))) >>
          diGraph.relations
            .traverse_(nr => Async[F].delay(writer.write(s"""   "${nr.from}" -> "${nr.to}" [${printAttributes(nr.attributes)}]\n"""))) >>
          Async[F].delay(writer.write("}"))

      }

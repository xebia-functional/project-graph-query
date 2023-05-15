package com.xebia.functional.graph.dot

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream

import java.io.{BufferedReader, FileReader, Reader}

trait DOTAlgebra[F[_]]:
  def parseDOT(reader: Reader): F[DiGraph]

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
        case NodeRegEx(name) => Node(name).asLeft.some
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

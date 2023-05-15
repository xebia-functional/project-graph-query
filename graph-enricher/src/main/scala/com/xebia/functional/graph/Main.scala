package com.xebia.functional.graph

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.xebia.functional.graph.csv.CSVLoader
import com.xebia.functional.graph.dot.DOTAlgebra
import com.xebia.functional.graph.instances.*

import java.io.FileReader
import java.net.URI
import java.nio.file.Path
import scala.util.control.NoStackTrace

object Main extends IOApp:

  case object InvalidArgsError extends RuntimeException("Expected args: <dotPath> <csvPath>") with NoStackTrace

  override def run(args: List[String]): IO[ExitCode] = {
    val readArg: IO[(Path, Path)] = args match
      case dotPath :: csvPath :: Nil =>
        IO(
          Path.of(URI.create(s"file://$dotPath")),
          Path.of(URI.create(s"file://$csvPath"))
        )
      case _ => InvalidArgsError.raiseError

    val dotParser: DOTAlgebra[IO] = DOTAlgebra.impl[IO]
    val csvLoader: CSVLoader[IO, ModuleData] = CSVLoader.impl[IO, ModuleData](using IO.asyncForIO, rowParser[IO])
    val sbtModuleGraphBuilder: SBTModuleGraphBuilder[IO] = SBTModuleGraphBuilder.impl[IO]
    val graphAnalyzer: GraphAnalyzer[IO] = GraphAnalyzer.impl[IO]

    def printLibrariesRelations(relations: List[SBTModuleRelation[ModuleData]]): String =
      relations
        .map(rel => s"${rel.from.name} -> ${rel.to.name} (${rel.relationType})")
        .mkString("\n")

    for
      paths <- readArg
      (dotPath, csvPath) = paths
      diGraph <- dotParser.parseDOT(new FileReader(dotPath.toFile))
      dataMap <- csvLoader.loadCSV(csvPath).map(m => (m.module, m)).compile.toList.map(_.toMap)
      graph <- sbtModuleGraphBuilder.createGraph(diGraph, n => dataMap.get(n.name))
      _ <- IO.println(s"Loaded ${graph.modules.size} modules and ${graph.relations.size} relations")
      // Find libraries that depends on non libraries
      relations = graph.relations.filter(r => r.from.extra.exists(_.moduleType.isLibrary) && r.to.extra.exists(d => !d.moduleType.isLibrary))
      _ <- if relations.isEmpty then IO.println("No libraries depending on non-libraries") else IO.println(printLibrariesRelations(relations))
      // Find subgraphs
      list <- graphAnalyzer.findConnected(graph).map(_.sortBy(_.size).reverse)
      _ <- IO.println(s"Found ${list.size} subgraphs")
      _ <- IO.println(s"Found ${list.count(_.size > 1)} subgraphs with more than one module")
      _ <- IO.println(list.map(_.map(m => s"${m.name} (${m.extra.map(_.moduleType).getOrElse("<unknown>")})").mkString(", ")).mkString("\n"))
    yield ExitCode.Success
  }

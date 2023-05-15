package com.xebia.functional.graph

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.xebia.functional.graph.csv.CSVLoader
import com.xebia.functional.graph.dot.{DOTAlgebra, DiGraph, Node, NodeRelation}
import com.xebia.functional.graph.instances.*

import java.io.{FileReader, FileWriter}
import java.net.URI
import java.nio.file.Path
import scala.annotation.tailrec
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

    def toDiGraphAttributes(moduleRelationType: SBTModuleRelationType): Map[String, String] =
      moduleRelationType match
        case SBTModuleRelationType.DependsOn(scopes) =>
          Map(
            "style" -> "solid",
            "extra" -> scopes.map(rs => s"${rs.origin.value}->${rs.target.value}").mkString(";")
          )
        case SBTModuleRelationType.Aggregates => Map("style" -> "dashed")

    def nodeShape(module: SBTModule[ModuleData]): Option[String] =
      module.extra.map(_.moduleType).map {
        case ModuleType.Microservice => "box3d"
        case ModuleType.Library => "folder"
        case ModuleType.Job => "circle"
        case ModuleType.Notification => "note"
        case ModuleType.Executable => "Msquare"
        case ModuleType.Aggregator => "tripleoctagon"
        case ModuleType.NUC => "polygon"
        case ModuleType.SparkJupiter => "tab"
      }

    def toDiGraph(fullGraph: SBTModuleGraph[ModuleData], include: Set[SBTModule[ModuleData]]): DiGraph = {
      val fullNodeSet = findFullNodeSet(include, include, fullGraph)
      DiGraph(
        fullNodeSet.toList.sortBy(_.name).map(m => Node(m.name, nodeShape(m))),
        fullGraph.relations
          .filter(mr => fullNodeSet.contains(mr.from)).map(mr => NodeRelation(mr.from.name, mr.to.name, toDiGraphAttributes(mr.relationType)))
      )
    }

    @tailrec
    def findFullNodeSet(
        current: Set[SBTModule[ModuleData]],
        findIn: Set[SBTModule[ModuleData]],
        fullGraph: SBTModuleGraph[ModuleData]
    ): Set[SBTModule[ModuleData]] = {
      val newSet = fullGraph.relations.filter(mr => findIn.exists(m => m.name == mr.from.name)).map(_.to).filterNot(current.contains).toSet
      if (newSet.isEmpty) current else findFullNodeSet(current ++ newSet, newSet, fullGraph)
    }

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
      _ <- list.zipWithIndex.traverse_ { case (set, index) =>
        val diGraph = toDiGraph(graph, set)
        dotParser.writeDOT(new FileWriter(s"target/dot-$index-graph.dot"), diGraph)
      }
    yield ExitCode.Success
  }

package com.xebia.functional.graph

import cats.effect.Async
import cats.syntax.all.*
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.alg.util.UnionFind
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}

import scala.jdk.CollectionConverters.*

trait GraphAnalyzer[F[_]]:
  def findConnected(graph: SBTModuleGraph[ModuleData]): F[List[Set[SBTModule[ModuleData]]]]

object GraphAnalyzer:
  def impl[F[_]: Async]: GraphAnalyzer[F] = new GraphAnalyzer[F]:
    override def findConnected(graph: SBTModuleGraph[ModuleData]): F[List[Set[SBTModule[ModuleData]]]] = {

      val modules = graph.modules
        .filter(_.name =!= "root")
        .filterNot(_.extra.exists(_.moduleType.isLibrary))

      val dependsOnRel: List[(SBTModule[ModuleData], SBTModule[ModuleData])] = graph.relations
        .filterNot(_.to.extra.exists(_.moduleType.isLibrary))
        .filter(_.relationType.fold(_ => true, false))
        .map(mr => (mr.from, mr.to))

      val initialSet: List[Set[SBTModule[ModuleData]]] = modules.map(Set(_))
      dependsOnRel
        .foldLeft(initialSet) { case (sets, (from, to)) =>
          val (contains, notContains) = sets.partition(s => s.contains(from) || s.contains(to))
          contains.fold[Set[SBTModule[ModuleData]]](Set.empty)((s1, s2) => s1 ++ s2) :: notContains
        }.pure[F]

//      val jgraph = new DefaultDirectedGraph[String, DefaultEdge](classOf[DefaultEdge])
//      graph.modules.filter(_.name =!= "root").foreach(m => jgraph.addVertex(m.name))
//      graph.relations
//        .filter(_.from.name =!= "root")
//        .filter(_.from.extra.exists(d => !d.moduleType.isLibrary))
//        .filter(_.relationType.fold(_ => true, false))
//        .foreach { rel =>
//          jgraph.addEdge(rel.from.name, rel.to.name)
//        }
//      val inspector = new ConnectivityInspector(jgraph)
//      inspector.connectedSets().asScala.toList.map(_.asScala.toSet).pure[F]
    }

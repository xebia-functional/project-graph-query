package com.xebia.functional.graph

import cats.syntax.all.*
import org.jgrapht.*
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.*
import org.jgrapht.traverse.*

import scala.jdk.CollectionConverters.*

object HelloJGraphT extends App:

  val graph =
    new DefaultDirectedGraph[SBTModule[Unit], SBTModuleRelationType](
      classOf[SBTModuleRelationType]
    )
  val module1 = SBTModule("name1", none[Unit])
  val module2 = SBTModule("name2", none[Unit])
  val module3 = SBTModule("name3", none[Unit])
  graph.addVertex(module1)
  graph.addVertex(module2)
  graph.addVertex(module3)
  graph.addEdge(module2, module1, SBTModuleRelationType.DependsOn(Nil))
  graph.addEdge(module3, module1, SBTModuleRelationType.DependsOn(Nil))

  graph.removeVertex(module1)
  val inspector = new ConnectivityInspector(graph)
  println(inspector.connectedSets().asScala.toList.size)

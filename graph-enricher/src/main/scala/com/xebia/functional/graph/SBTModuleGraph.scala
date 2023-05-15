package com.xebia.functional.graph

import cats.effect.kernel.Async
import cats.syntax.all.*
import com.xebia.functional.graph.dot.*

trait SBTModuleGraphBuilder[F[_]]:
  def createGraph[A](diGraph: DiGraph, enricher: Node => Option[A]): F[SBTModuleGraph[A]]

object SBTModuleGraphBuilder:

  private val scopeRegEx = "(compile|test)->(compile|test)".r

  def impl[F[_]: Async]: SBTModuleGraphBuilder[F] = new SBTModuleGraphBuilder[F]:
    def createGraph[A](
        diGraph: DiGraph,
        enricher: Node => Option[A]
    ): F[SBTModuleGraph[A]] = {

      def findScopes(raw: String): List[RelationScope] =
        scopeRegEx.findAllMatchIn(raw).toList.flatMap { regExMatch =>
          (
            DependsOnScope.from(regExMatch.group(1)),
            DependsOnScope.from(regExMatch.group(2)),
          ).mapN(RelationScope.apply).toList
        }

      def relationType(attr: Map[String, String]): Option[SBTModuleRelationType] =
        attr.get("style") match
          case Some("solid") =>
            val scopes = attr.get("extra").toList.flatMap(findScopes)
            SBTModuleRelationType.DependsOn(scopes).some
          case Some("dashed") =>
            SBTModuleRelationType.Aggregates.some
          case _ => None

      val modulesMap = diGraph.nodes.map { node =>
        node.name -> SBTModule(node.name, enricher(node))
      }.toMap
      val relations = diGraph.relations.flatMap { nodeRelation =>
        (
          modulesMap.get(nodeRelation.from),
          modulesMap.get(nodeRelation.to),
          relationType(nodeRelation.attributes)
        ).mapN(SBTModuleRelation.apply)
      }
      SBTModuleGraph(modulesMap.values.toList, relations).pure[F]
    }

package com.xebia.functional.graph

import cats.Show
import cats.syntax.all.*

final case class SBTModuleGraph[A](
    modules: List[SBTModule[A]],
    relations: List[SBTModuleRelation[A]]
)
final case class SBTModule[A](name: String, extra: Option[A])
final case class SBTModuleRelation[A](
    from: SBTModule[A],
    to: SBTModule[A],
    relationType: SBTModuleRelationType
)

sealed trait SBTModuleRelationType extends Product with Serializable:
  def fold[A](dependsOn: SBTModuleRelationType.DependsOn => A, aggregates: => A): A =
    this match
      case r: SBTModuleRelationType.DependsOn => dependsOn(r)
      case SBTModuleRelationType.Aggregates => aggregates
object SBTModuleRelationType:
  final case class DependsOn(scopes: List[RelationScope]) extends SBTModuleRelationType
  case object Aggregates extends SBTModuleRelationType

sealed trait DependsOnScope extends Product with Serializable
object DependsOnScope:
  case object Compile extends DependsOnScope
  case object Test extends DependsOnScope
  def from(s: String): Option[DependsOnScope] = s match
    case "compile" => Compile.some
    case "test" => Test.some
    case _ => None

final case class RelationScope(origin: DependsOnScope, target: DependsOnScope)

final case class ModuleData(module: String, moduleType: ModuleType, path: String, deployable: Boolean)
sealed trait ModuleType extends Product with Serializable:
  def isLibrary: Boolean = this match
    case ModuleType.Library => true
    case _ => false
object ModuleType:
  case object Microservice extends ModuleType
  case object Library extends ModuleType
  case object Job extends ModuleType
  case object Notification extends ModuleType
  case object Executable extends ModuleType
  case object Aggregator extends ModuleType
  case object NUC extends ModuleType
  case object SparkJupiter extends ModuleType

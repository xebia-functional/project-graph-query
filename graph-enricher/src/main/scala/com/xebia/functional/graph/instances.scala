package com.xebia.functional.graph

import cats.MonadThrow
import cats.syntax.all.*
import com.xebia.functional.graph.csv.RowParser

import scala.util.control.NoStackTrace

object instances:

  final case class InvalidRowFieldError(msg: String) extends RuntimeException(msg) with NoStackTrace

  def moduleTypeFromString(s: String): Either[InvalidRowFieldError, ModuleType] = s match {
    case "JOB" => ModuleType.Job.asRight
    case "MSV" => ModuleType.Microservice.asRight
    case "LIB" => ModuleType.Library.asRight
    case "NOT" => ModuleType.Notification.asRight
    case "EXE" => ModuleType.Executable.asRight
    case "AGG" => ModuleType.Aggregator.asRight
    case "NUC" => ModuleType.NUC.asRight
    case "SPJ" => ModuleType.SparkJupiter.asRight
    case s => InvalidRowFieldError(s"Invalid value for ModuleType: '$s'").asLeft
  }

  def deployableFromString(s: String): Either[InvalidRowFieldError, Boolean] = s match
    case "TRUE" => true.asRight
    case "FALSE" => false.asRight
    case _ => InvalidRowFieldError(s"Invalid value for Deployable: '$s'").asLeft

  def rowParser[F[_]: MonadThrow]: RowParser[F, ModuleData] = new RowParser[F, ModuleData] {
    def parse(row: List[String]): F[ModuleData] =
      row match
        case name :: moduleType :: path :: deployable :: _ =>
          (
            MonadThrow[F].fromEither(moduleTypeFromString(moduleType)),
            MonadThrow[F].fromEither(deployableFromString(deployable))
          ).mapN((mt, d) => ModuleData(name, mt, path, d))
        case _ => InvalidRowFieldError(s"Wrong number of columns: ${row.size}").raiseError
  }

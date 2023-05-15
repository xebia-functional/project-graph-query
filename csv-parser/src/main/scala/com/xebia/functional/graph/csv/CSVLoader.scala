package com.xebia.functional.graph.csv

import cats.effect.Async
import cats.syntax.all.*
import com.github.tototoshi.csv.*
import fs2.Stream
import fs2.data.csv.*
import fs2.data.csv.generic.semiauto.*
import fs2.io.file.Files

import java.io.{File, FileReader}
import java.nio.file.Path

trait CSVLoader[F[_], A]:
  def loadCSV(path: Path): Stream[F, A]

object CSVLoader:
  def impl[F[_]: Async, A](using p: RowParser[F, A]): CSVLoader[F, A] =
    new CSVLoader[F, A]:
      def loadCSV(path: Path): Stream[F, A] =
        // TODO - Use iterator instead (that was giving me just the first row)
        Stream
          .fromAutoCloseable(Async[F].delay(CSVReader.open(new FileReader(path.toFile))))
          .evalMap(reader => Async[F].delay(reader.all()))
          .flatMap(Stream.emits)
          .evalMap(p.parse)

package com.xebia.functional.graph.csv

trait RowParser[F[_], A]:
  def parse(row: List[String]): F[A]

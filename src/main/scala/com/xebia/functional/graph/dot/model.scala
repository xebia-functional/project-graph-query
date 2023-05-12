package com.xebia.functional.graph.dot

import cats.Show

final case class DiGraph(nodes: List[Node], relations: List[NodeRelation]):
  def :+(n: Node): DiGraph = copy(nodes = nodes :+ n)
  def :+(nr: NodeRelation): DiGraph = copy(relations = relations :+ nr)

final case class Node(name: String)
object Node:
  given Show[Node] = Show.show(n => s"""Node(name: "${n.name}")""")
final case class NodeRelation(from: String, to: String, attributes: Map[String, String])
object NodeRelation:
  private def printAttributes(attributes: Map[String, String]): String =
    attributes.toList.map((s1, s2) => s"\"$s1\" -> \"$s2\"").mkString(", ")

  given Show[NodeRelation] = Show.show { nr =>
    s"""NodeRelation(
       |  from: "${nr.from}",
       |  to: "${nr.to}",
       |  attributes: ${printAttributes(nr.attributes)}
       |)""".stripMargin
  }

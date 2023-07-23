package com.axidraw

import cats.data.EitherT
import cats.implicits._
import scala.annotation.tailrec

/**
 * Represents a directed graph that represents the pen movement for a drawing.
 *
 * @param nodes The nodes of the graph, each corresponding to a point in the drawing.
 * @param edges The edges of the graph, each representing a pen movement between two points.
 */
case class Plan(val nodes: List[Point], val edges: List[Segment]) {}

object Plan {

  /**
   * Creates a new Plan with a list of geometries.
   *
   * This method takes a list of geometries and constructs a directed graph (Plan) representing
   * the pen movement for the entire drawing. The graph will consist of nodes, where each node
   * corresponds to a point in the drawing, and edges representing the pen movement between two points.
   *
   * The construction of the graph follows a greedy approach, where at each step, the "nearest" geometry
   * to the current end point is added to the graph. The "nearest" geometry is determined based on the
   * Euclidean distance between points. This process continues until all geometries are added to the graph.
   *
   * Additionally, the graph will include additional edges with pen-up movements that are not part of
   * the original geometries. These additional edges are used to connect the end point of one geometry
   * to the start point of the next one in the path, representing "pen-up" movements where the pen is not
   * touching the paper.
   *
   * @param geometries The list of geometries to initialize the Plan.
   * @return Either an error if creating the Plan fails, or the new Plan instance.
   */
  def apply(geometries: List[Geometry[_]]): Either[GeometryError, Plan] = ???

  /**
   * Creates a new Plan with a Drawing.
   *
   * This method takes a Drawing containing multiple geometries and constructs a directed graph (Plan)
   * representing the pen movement for the entire drawing. The graph will consist of nodes, where each node
   * corresponds to a point in the drawing, and edges representing the pen movement between two points.
   *
   * The construction of the graph follows a greedy approach, where at each step, the "nearest" geometry
   * to the current end point is added to the graph. The "nearest" geometry is determined based on the
   * Euclidean distance between points. This process continues until all geometries are added to the graph.
   *
   * Additionally, the graph will include additional edges with pen-up movements that are not part of
   * the original geometries. These additional edges are used to connect the end point of one geometry
   * to the start point of the next one in the path, representing "pen-up" movements where the pen is not
   * touching the paper.
   *
   * @param drawing The drawing containing multiple geometries to initialize the Plan.
   * @return Either an error if creating the Plan fails, or the new Plan instance.
   */
  def apply(drawing: Drawing): Either[GeometryError, Plan] = {
    val geometries = drawing.geometries.toList
    Plan(geometries)
  }

}

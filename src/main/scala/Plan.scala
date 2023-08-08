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
case class Plan(val nodes: List[Point], val edges: List[Segment]) {

  /**
   * Adds a new geometry to the plan.
   *
   * This method takes a geometry and calculates the path segments that represent the pen movements for this geometry.
   * It appends these new segments to the existing list of segments in the plan.
   *
   * If the input geometry is a polygon, this method first transforms it into a list of rings (exterior and interiors).
   * For each ring, it creates new path segments from the ring's points. It also creates an additional "pen-up" path
   * segment to move the pen from the last point of the previous geometry (or ring) to the first point of the current ring,
   * if the last point is available.
   *
   * If the input geometry is not a polygon, this method creates new path segments from the geometry's points.
   * Similar to the polygon case, it creates an additional "pen-up" path segment to move the pen from the last point of
   * the previous geometry to the first point of the current geometry, if the last point is available.
   *
   * @param geometry The new geometry to add to the plan.
   * @return Either an error if the operation fails, or a new Plan instance with the added geometry.
   */
  def addGeometry(geometry: Geometry[_]): Either[GeometryError, Plan] = {
    val lastPoint = nodes.lastOption
    geometry match {
      case polygon: Polygon =>
        val allRings = polygon.exteriorRing :: polygon.interiorRings
        val newSegments = allRings.flatMap { ring =>
          val points = ring.coords.getOrElse(List())
          val ringSegments = points.sliding(2).map(p => Segment(p.head, p.last)).toList
          val moveToNextRingSegment = lastPoint.map(lp => Segment(lp, points.head, penUp = true, speed = 1))
          moveToNextRingSegment.toList ++ ringSegments
        }
        val newNodes = newSegments.flatMap(segment => List(segment.from, segment.to))
        Right(copy(nodes = nodes ++ newNodes, edges = edges ++ newSegments))
      case _ =>
        geometry.coords match {
          case Left(error) => Left(error)
          case Right(points) =>
            val newSegments = (lastPoint, points) match {
              case (Some(last), h :: _) =>
                Segment(last, h, penUp = true, speed = 1) :: points.sliding(2).map(p => Segment(p.head, p.last)).toList
              case (_, ps) => ps.sliding(2).map(p => Segment(p.head, p.last)).toList
            }
            val newNodes = newSegments.flatMap(segment => List(segment.from, segment.to))
            Right(copy(nodes = nodes ++ newNodes, edges = edges ++ newSegments))
        }
    }
  }

  /**
   * Returns the list of segments (edges) in the plan in their current order.
   *
   * @return The ordered list of segments in the plan.
   */
  def getSegments: List[Segment] = edges

}

object Plan {

  /**
   * Creates a new Plan with a list of geometries.
   *
   * This method takes a list of geometries and constructs a Plan representing
   * the pen movement for the entire drawing. The Plan is constructed by sequentially
   * adding each geometry in the list to the Plan using the `addGeometry` method.
   * The order of the geometries in the list is preserved in the Plan.
   *
   * If any call to `addGeometry` fails (i.e., returns a Left), the error is immediately
   * returned and no further geometries are processed. If all geometries are successfully
   * added, the resulting Plan is returned.
   *
   * @param geometries The list of geometries to initialize the Plan.
   * @return Either an error if creating the Plan fails, or the new Plan instance.
   */
  def apply(geometries: List[Geometry[_]]): Either[GeometryError, Plan] =
    geometries.foldLeft(Right(Plan(List(), List())): Either[GeometryError, Plan]) { (plan, geometry) =>
      plan.flatMap(_.addGeometry(geometry))
    }

  /**
   * Creates a new Plan with a Drawing.
   *
   * This method takes a Drawing and constructs a Plan representing
   * the pen movement for the entire drawing. The Plan is constructed by converting
   * the Drawing's geometries to a list and then calling the `apply` method that
   * takes a list of geometries.
   *
   * The order of the geometries in the Drawing is preserved in the Plan.
   *
   * @param drawing The drawing containing multiple geometries to initialize the Plan.
   * @return Either an error if creating the Plan fails, or the new Plan instance.
   */
  def apply(drawing: Drawing): Either[GeometryError, Plan] =
    apply(drawing.geometries.toList)

}

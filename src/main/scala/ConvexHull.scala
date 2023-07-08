package com.axidraw

import cats.Order
import cats.instances.list.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

import scala.math.Ordering

/**
 * Computes the convex hull of a set of 2D points using the Graham's scan algorithm.
 */
object ConvexHull {

  /**
   * Computes the convex hull of the given list of points.
   *
   * @param points the list of points
   * @return the convex hull of the points
   */
  def convexHull(points: List[Point]): List[Point] = {
    // Find the point with the lowest y-coordinate (and the leftmost one if there's a tie)
    val lowestPoint = points.minBy(p => (p.y, p.x))

    // Sort the points by their polar angles with respect to the lowest point
    val sortedPoints = points.sortBy(p => (polarAngle(lowestPoint, p), p.distance(lowestPoint)))

    // Compute the convex hull using a monadic fold
    sortedPoints.foldLeftM(List.empty[Point]) { (stack, point) =>
      for {
        updatedStack <- addPointToStack(stack, point)
        finalStack <- removeCollinearPoints(updatedStack)
      } yield finalStack
    }.getOrElse(Nil).reverse
  }

  /**
   * Calculates the polar angle between two points.
   *
   * @param origin the origin point
   * @param point  the target point
   * @return the polar angle between the two points
   */
  private def polarAngle(origin: Point, point: Point): Double =
    math.atan2(point.y - origin.y, point.x - origin.x)

  /**
   * Checks if three points are in clockwise order.
   *
   * @param p1 the first point
   * @param p2 the second point
   * @param p3 the third point
   * @return `true` if the points are in clockwise order, `false` otherwise
   */
  private def isClockwise(p1: Point, p2: Point, p3: Point): Boolean =
    (p2.x - p1.x) * (p3.y - p1.y) - (p3.x - p1.x) * (p2.y - p1.y) < 0

  /**
   * Checks if three points are collinear.
   *
   * @param p1 the first point
   * @param p2 the second point
   * @param p3 the third point
   * @return `true` if the points are collinear, `false` otherwise
   */
  private def isCollinear(p1: Point, p2: Point, p3: Point): Boolean =
    (p2.x - p1.x) * (p3.y - p1.y) == (p3.x - p1.x) * (p2.y - p1.y)

  /**
   * Adds a point to the stack while maintaining convexity.
   *
   * @param stack the current stack of points
   * @param point the point to add
   * @return an option containing the updated stack, or `None` if the point violates convexity
   */
  private def addPointToStack(stack: List[Point], point: Point): Option[List[Point]] =
    stack match {
      case p2 :: p1 :: rest if isClockwise(p1, p2, point) =>
        Some(addPointToStack(p1 :: rest, point).getOrElse(Nil))
      case _ =>
        Some(point :: stack)
    }

  /**
   * Removes collinear points from the stack.
   *
   * @param points the stack of points
   * @return an option containing the stack with collinear points removed, or `None` if no removal is necessary
   */
  private def removeCollinearPoints(points: List[Point]): Option[List[Point]] =
    points match {
      case p3 :: p2 :: p1 :: rest if isCollinear(p1, p2, p3) =>
        Some(removeCollinearPoints(p3 :: p1 :: rest).getOrElse(Nil))
      case _ =>
        Some(points)
    }
}

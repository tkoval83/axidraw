package com.axidraw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConvexHullSpec extends AnyFlatSpec with Matchers {

  "ConvexHull" should "return the correct convex hull" in {
    val points = List(
      Point(12, 32),
      Point(45, 98),
      Point(65, 12),
      Point(10, 30)
    )
    val convexHull = ConvexHull.convexHull(points)
    convexHull should contain theSameElementsAs List(
      Point(10, 30),
      Point(45, 98),
      Point(65, 12)
    )
  }

  it should "handle a single point" in {
    val singlePoint = List(Point(1, 1))
    assert(ConvexHull.convexHull(singlePoint) == singlePoint)
  }

  it should "handle two points" in {
    val twoPoints = List(Point(1, 1), Point(2, 2))
    assert(ConvexHull.convexHull(twoPoints) == twoPoints)
  }

  it should "handle three collinear points" in {
    val collinearPoints = List(Point(1.0, 1.0), Point(2.0, 2.0), Point(3.0, 3.0))
    assert(ConvexHull.convexHull(collinearPoints) == List(Point(1.0, 1.0), Point(3.0, 3.0)))
  }

  it should "handle four points forming a rectangle" in {
    val rectanglePoints = List(Point(0, 0), Point(0, 5), Point(5, 5), Point(5, 0))
    val convexHull = ConvexHull.convexHull(rectanglePoints)
    convexHull should contain theSameElementsAs rectanglePoints
  }

  it should "handle multiple points forming a convex polygon" in {
    val convexPolygonPoints = List(Point(0, 0), Point(0, 5), Point(3, 7), Point(5, 5), Point(5, 0))
    val convexHull = ConvexHull.convexHull(convexPolygonPoints)
    convexHull should contain theSameElementsAs convexPolygonPoints
  }

  it should "handle multiple points forming a concave polygon" in {
    val concavePolygonPoints = List(Point(0, 0), Point(0, 5), Point(3, 7), Point(5, 3), Point(5, 0))
    val convexHull = ConvexHull.convexHull(concavePolygonPoints)
    convexHull should contain theSameElementsAs List(
      Point(0, 0),
      Point(0, 5),
      Point(3, 7),
      Point(5, 3),
      Point(5, 0)
    )
  }

  it should "handle multiple points with duplicate points" in {
    val pointsWithDuplicates = List(Point(1, 1), Point(2, 2), Point(1, 1), Point(3, 3), Point(2, 2))
    val convexHull = ConvexHull.convexHull(pointsWithDuplicates)
    convexHull should contain theSameElementsAs List(Point(1, 1), Point(3, 3))
  }

}

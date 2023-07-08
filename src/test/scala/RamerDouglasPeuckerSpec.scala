package com.axidraw

import com.axidraw.{Point, RamerDouglasPeucker}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RamerDouglasPeuckerSpec extends AnyWordSpec with Matchers {

  "RamerDouglasPeucker" should {

    "simplify a line string" in {
      val points = List(
        Point(0, 0),
        Point(1, 0.1),
        Point(2, -0.1),
        Point(3, 5),
        Point(4, 6),
        Point(5, 7),
        Point(6, 8.1),
        Point(7, 9),
        Point(8, 9),
        Point(9, 9)
      )
      val epsilon = 1.0
      val simplifiedPoints = RamerDouglasPeucker.simplify(points, epsilon)
      simplifiedPoints shouldEqual List(Point(0, 0), Point(2, -0.1), Point(3, 5), Point(7, 9), Point(9, 9))
    }

    "return the original line string if it cannot be simplified further" in {
      val points = List(Point(0, 0), Point(2, 0))
      val epsilon = 1.0
      val simplifiedPoints = RamerDouglasPeucker.simplify(points, epsilon)
      simplifiedPoints shouldEqual points
    }

  }

}

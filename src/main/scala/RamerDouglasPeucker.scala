package com.axidraw

/**
 * The Ramer-Douglas-Peucker algorithm simplifies a curve and removes unnecessary points.
 */
object RamerDouglasPeucker {

  /**
   * The Ramer-Douglas-Peucker algorithm simplifies a curve and removes unnecessary points.
   *
   * @param points  the original line string points
   * @param epsilon the tolerance that determines the degree of simplification
   * @return the simplified line string
   */
  def simplify(points: List[Point], epsilon: Double): List[Point] = {
    def rdp(line: List[Point], epsilon: Double): List[Point] = {
      def perpendicularDistance(point: Point, start: Point, end: Point): Double = {
        val a = end.y - start.y
        val b = start.x - end.x
        val c = (end.x * start.y) - (start.x * end.y)
        val divisor = math.sqrt((a * a) + (b * b))
        math.abs((a * point.x) + (b * point.y) + c) / divisor
      }

      val startIdx = 0
      val endIdx = line.length - 1
      var maxDist = 0.0
      var maxId = 0

      for (i <- startIdx + 1 until endIdx) {
        val d = perpendicularDistance(line(i), line(startIdx), line(endIdx))
        if (d > maxDist) {
          maxDist = d
          maxId = i
        }
      }

      if (maxDist > epsilon) {
        val left = rdp(line.slice(startIdx, maxId + 1), epsilon)
        val right = rdp(line.slice(maxId, endIdx + 1), epsilon)
        left.init ++ right
      } else {
        List(line.head, line.last)
      }
    }

    rdp(points, epsilon)
  }
}

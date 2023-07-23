package com.axidraw

/**
 * Represents the bounding box of a set of points in 2D space.
 *
 * @param minPoint The minimum point (lower-left corner) of the bounding box.
 * @param maxPoint The maximum point (upper-right corner) of the bounding box.
 */
case class Bounds(minPoint: Point, maxPoint: Point)

package com.axidraw

import com.axidraw.Point

/**
 * A class representing a path segment between two points in the drawing.
 * A segment can be either with the pen up (not drawing) or down (drawing).
 *
 * @param from The starting point of the path.
 * @param to The ending point of the path.
 * @param penUp Indicates whether the pen should be up (true) or down (false). If true, the path will not result in a drawn line.
 * @param speed The speed at which to draw the path. This can be used to control the quality and appearance of the drawing.
 */
case class Segment(from: Point, to: Point, penUp: Boolean = false, speed: Int = 1)

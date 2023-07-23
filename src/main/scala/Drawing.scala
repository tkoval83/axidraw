package com.axidraw

import cats.effect.IO
import java.awt.image.BufferedImage

/**
 * Represents a drawing composed of geometric paths.
 *
 * @param geometries The list of geometries representing the drawing.
 * @param width      The width of the drawing.
 * @param height     The height of the drawing.
 */
case class Drawing(geometries: List[Geometry[_]], width: Int, height: Int) {

  /**
   * Calculates the bounds of the drawing.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the drawing.
   */
  def bounds: Either[GeometryError, Bounds] = {
    val boundsList = geometries.flatMap(_.bounds.toOption)
    boundsList match {
      case Nil =>
        Left(GeometryError.EmptyGeometry("Cannot calculate bounds of an empty GeometryCollection."))
      case nonEmptyBounds =>
        val minX = nonEmptyBounds.map(_.minPoint.x).min
        val maxX = nonEmptyBounds.map(_.maxPoint.x).max
        val minY = nonEmptyBounds.map(_.minPoint.y).min
        val maxY = nonEmptyBounds.map(_.maxPoint.y).max
        Right(Bounds(Point(minX, minY), Point(maxX, maxY)))
    }
  }

  /**
   * Translates the drawing by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated drawing.
   */
  def translate(dx: Double, dy: Double): Either[GeometryError, Drawing] = ???

  /**
   * Rotates the drawing by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated drawing.
   */
  def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, Drawing] = ???

  /**
   * Scales the drawing by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled drawing.
   */
  def scale(factor: Double): Either[GeometryError, Drawing] = ???

  /**
   * Simplifies the drawing by reducing the number of vertices or control points.
   *
   * @param tolerance The tolerance parameter for simplification.
   * @return Either an error if simplifying the drawing fails, or the simplified drawing.
   */
  def simplify(tolerance: Double): Either[GeometryError, Drawing] = ???

  /**
   * Fits the drawing within the specified bounds.
   *
   * @return Either an error if fitting the drawing fails, or the fitted drawing.
   */
  def fit(): Either[GeometryError, Drawing] = ???

  /**
   * Transforms the drawing using the specified transformation function.
   *
   * @param f The transformation function to apply.
   * @return Either an error if transforming the drawing fails, or the transformed drawing.
   */
  def transform(f: List[Geometry[_]] => Either[GeometryError, List[Geometry[_]]]): Either[GeometryError, Drawing] = ???

  /**
   * Generates an image representation of the drawing using Java2D.
   *
   * @return An `IO` that represents the generation of the image.
   */
  def generateImage(): IO[BufferedImage] = ???
}

package com.axidraw

import cats.data.{EitherT, ValidatedNec}
import cats.implicits.*
import cats.instances.list.*
import cats.syntax.traverse.*

import scala.annotation.tailrec
import scala.math.atan2

/**
 * The `Geometry` trait represents a geometric shape in a 2D space.
 * It provides methods for common geometric operations.
 */
sealed trait Geometry[A <: Geometry[A]]:

  /**
   * Translates the geometry by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated geometry.
   */
  def translate(dx: Double, dy: Double): Either[GeometryError, A]

  /**
   * Rotates the geometry by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated geometry.
   */
  def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, A]

  /**
   * Scales the geometry by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled geometry.
   */
  def scale(factor: Double): Either[GeometryError, A]

  /**
   * Returns the coordinates of the geometry as a list of points.
   *
   * @return Either an error if retrieving the coordinates fails, or the list of coordinates.
   */
  def coords: Either[GeometryError, List[Point]]

  /**
   * Checks if the geometry is valid.
   *
   * @return Either an error if validating the geometry fails, or a boolean indicating validity.
   */
  def isValid: Either[GeometryError, Boolean]

  /**
   * Decomposes the geometry into a list of simpler geometries.
   *
   * @return Either an error if decomposing the geometry fails, or the list of decomposed geometries.
   */
  def decompose: Either[GeometryError, List[A]]

  /**
   * Simplifies the geometry by reducing the number of vertices or control points.
   *
   * @param tolerance The tolerance parameter for simplification.
   * @return Either an error if simplifying the geometry fails, or the simplified geometry.
   */
  def simplify(tolerance: Double): Either[GeometryError, A]

  /**
   * Calculates the bounds of the geometry.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the geometry.
   */
  def bounds: Either[GeometryError, Bounds]

/**
 * Represents a 2D point in geometric space.
 *
 * @param x The x-coordinate of the point.
 * @param y The y-coordinate of the point.
 */
case class Point(x: Double, y: Double) extends Geometry[Point]:

  /**
   * Calculates the Euclidean distance between this point and the given other point.
   *
   * The Euclidean distance between two points in a 2D Cartesian coordinate system is given by the Pythagorean theorem:
   * distance = sqrt((x2 - x1)^2 + (y2 - y1)^2), where (x1, y1) are the coordinates of this point and (x2, y2) are the coordinates of the other point.
   *
   * @param other The other point to which the distance is calculated.
   * @return The Euclidean distance between this point and the other point.
   */
  def distance(other: Point): Double = {
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }

  /**
   * Computes the shortest distance from this point to a line segment defined by two other points.
   *
   * @param pointA One end of the line segment.
   * @param pointB The other end of the line segment.
   * @return The shortest distance from this point to the line segment.
   */
  def distanceToLine(pointA: Point, pointB: Point): Double = {
    val area = Math.abs(0.5 * (pointA.x * (pointB.y - y) + pointB.x * (y - pointA.y) + x * (pointA.y - pointB.y)))
    val lineLength = pointA.distance(pointB)
    area / lineLength
  }

  /**
   * Translates the point by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated point.
   */
  override def translate(dx: Double, dy: Double): Either[GeometryError, Point] =
    Right(copy(x = x + dx, y = y + dy))

  /**
   * Rotates the point by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated point.
   */
  override def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, Point] = {
    val radians = math.toRadians(angle)
    val translated = translate(-origin.x, -origin.y)
    translated.flatMap(_.rotateInternal(radians)).flatMap(_.translate(origin.x, origin.y))
  }

  private def rotateInternal(angle: Double): Either[GeometryError, Point] = {
    val cos = math.cos(angle)
    val sin = math.sin(angle)
    val newX = x * cos - y * sin
    val newY = x * sin + y * cos
    Right(Point(newX, newY))
  }

  /**
   * Scales the point by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled point.
   */
  override def scale(factor: Double): Either[GeometryError, Point] =
    Right(copy(x = x * factor, y = y * factor))

  /**
   * Returns the coordinates of the point as a list containing only itself.
   *
   * @return Either an error if retrieving the coordinates fails, or the list containing the point itself.
   */
  override def coords: Either[GeometryError, List[Point]] =
    Right(List(this))

  /**
   * Checks if the point is valid.
   *
   * @return Either an error if validating the point fails, or a boolean indicating validity.
   */
  override def isValid: Either[GeometryError, Boolean] =
    Right(true)

  /**
   * Decomposes the point into a list containing only itself.
   *
   * @return Either an error if decomposing the point fails, or the list containing the point itself.
   */
  override def decompose: Either[GeometryError, List[Point]] =
    Right(List(this))

  /**
   * Simplifies the point (no effect).
   *
   * @param tolerance The tolerance parameter for simplification (not used).
   * @return Either an error if simplifying the point fails, or the point itself.
   */
  override def simplify(tolerance: Double): Either[GeometryError, Point] =
    Right(this)

  /**
   * Calculates the bounds of the geometry.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the geometry.
   */
  def bounds: Either[GeometryError, Bounds] =
    Right(Bounds(this, this))

/**
 * Represents a LineString in geometric space.
 *
 * @param points The list of points that make up the LineString.
 */
case class LineString(points: List[Point]) extends Geometry[LineString] {

  /**
   * Translates the LineString by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated LineString.
   */
  override def translate(dx: Double, dy: Double): Either[GeometryError, LineString] = {
    val translatedPoints = points.traverse(_.translate(dx, dy))
    translatedPoints.map(LineString)
  }

  /**
   * Rotates the LineString by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated LineString.
   */
  override def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, LineString] = {
    val rotatedPoints = points.traverse(_.rotate(angle, origin))
    rotatedPoints.map(LineString)
  }

  /**
   * Scales the LineString by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled LineString.
   */
  override def scale(factor: Double): Either[GeometryError, LineString] = {
    val scaledPoints = points.traverse(_.scale(factor))
    scaledPoints.map(LineString)
  }

  /**
   * Returns the coordinates of the LineString as a list of points.
   *
   * @return Either an error if retrieving the coordinates fails, or the list of coordinates.
   */
  override def coords: Either[GeometryError, List[Point]] =
    Right(points)

  /**
   * Checks if the LineString is valid.
   *
   * @return Either an error if validating the LineString fails, or a boolean indicating validity.
   */
  override def isValid: Either[GeometryError, Boolean] =
    Right(points.size >= 2)

  /**
   * Decomposes the LineString into a list of simpler geometries.
   *
   * @return Either an error if decomposing the LineString fails, or the list of decomposed geometries.
   */
  override def decompose: Either[GeometryError, List[LineString]] =
    Right(List(this))

  /**
   * Simplifies the LineString by reducing the number of vertices or control points.
   *
   * @param tolerance The tolerance parameter for simplification.
   * @return Either an error if simplifying the LineString fails, or the simplified LineString.
   */
  override def simplify(tolerance: Double): Either[GeometryError, LineString] = {
    val simplifiedPoints = RamerDouglasPeucker.simplify(points, tolerance)
    Right(LineString(simplifiedPoints))
  }

  /**
   * Calculates the bounds of the LineString using the convex hull algorithm.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the LineString.
   */
  def bounds: Either[GeometryError, Bounds] =
    if (points.isEmpty) {
      Left(GeometryError.InvalidLineString("Cannot calculate bounds of an empty LineString."))
    } else {
      val convexHull = ConvexHull.convexHull(points)
      val minX = convexHull.minBy(_.x).x
      val maxX = convexHull.maxBy(_.x).x
      val minY = convexHull.minBy(_.y).y
      val maxY = convexHull.maxBy(_.y).y
      Right(Bounds(Point(minX, minY), Point(maxX, maxY)))
    }

  }

/**
 * Represents a Polygon in 2D geometric space.
 *
 * A Polygon is a planar Surface representing a multi-sided geometry. It is defined by a single exterior boundary
 * and zero or more interior boundaries, each represented by a LineString. The exterior LineString defines the
 * outer boundary of the Polygon, and each interior LineString defines a hole in the Polygon.
 *
 * The exterior and interior LineStrings of a Polygon should be simple (not self-intersecting) and closed
 * (start and end point of the LineString are the same point).
 *
 * @param exteriorRing  The exterior LineString that represents the outer boundary of the Polygon.
 * @param interiorRings The list of interior LineStrings that represent the holes in the Polygon. Defaults to an empty list.
 */
case class Polygon(exteriorRing: LineString, interiorRings: List[LineString] = Nil) extends Geometry[Polygon] {

  /**
   * Translates the Polygon by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated Polygon.
   */
  override def translate(dx: Double, dy: Double): Either[GeometryError, Polygon] =
    for {
      translatedExterior <- exteriorRing.translate(dx, dy)
      translatedInterior <- interiorRings.traverse(_.translate(dx, dy))
    } yield Polygon(translatedExterior, translatedInterior)

  /**
   * Rotates the Polygon by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated Polygon.
   */
  override def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, Polygon] =
    for {
      rotatedExterior <- exteriorRing.rotate(angle, origin)
      rotatedInterior <- interiorRings.traverse(_.rotate(angle, origin))
    } yield Polygon(rotatedExterior, rotatedInterior)

  /**
   * Scales the Polygon by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled Polygon.
   */
  override def scale(factor: Double): Either[GeometryError, Polygon] =
    for {
      scaledExterior <- exteriorRing.scale(factor)
      scaledInterior <- interiorRings.traverse(_.scale(factor))
    } yield Polygon(scaledExterior, scaledInterior)

  /**
   * Returns the coordinates of the Polygon as a list of points.
   *
   * @return Either an error if retrieving the coordinates fails, or the list of coordinates.
   */
  override def coords: Either[GeometryError, List[Point]] =
    Right(exteriorRing.points)

  /**
   * Checks if the Polygon is valid.
   *
   * @return Either an error if validating the Polygon fails, or a boolean indicating validity.
   */
  override def isValid: Either[GeometryError, Boolean] = {
    val exteriorIsValid = exteriorRing.isValid
    val interiorsAreValid = interiorRings.forall(_.isValid.getOrElse(false))
    if (exteriorIsValid.getOrElse(false) && interiorsAreValid)
      Right(true)
    else
      Left(GeometryError.InvalidPolygon("Invalid polygon"))
  }

  /**
   * Decomposes the Polygon into a list of simpler geometries.
   *
   * @return Either an error if decomposing the Polygon fails, or the list of decomposed geometries.
   */
  override def decompose: Either[GeometryError, List[Polygon]] =
    Right(List(this))

  /**
   * Simplifies the Polygon by reducing the number of vertices or control points.
   *
   * @param tolerance The tolerance parameter for simplification.
   * @return Either an error if simplifying the Polygon fails, or the simplified Polygon.
   */
  override def simplify(tolerance: Double): Either[GeometryError, Polygon] =
    for {
      simplifiedExterior <- exteriorRing.simplify(tolerance)
      simplifiedInteriors <- interiorRings.traverse(_.simplify(tolerance))
    } yield Polygon(simplifiedExterior, simplifiedInteriors)

  /**
   * Calculates the bounds of the Polygon using the convex hull algorithm.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the Polygon.
   */
  def bounds: Either[GeometryError, Bounds] = {
    val allPoints = exteriorRing.points
    if (allPoints.isEmpty) {
      Left(GeometryError.InvalidPolygon("Cannot calculate bounds of an empty Polygon."))
    } else {
      val convexHull = ConvexHull.convexHull(allPoints)
      val minX = convexHull.minBy(_.x).x
      val maxX = convexHull.maxBy(_.x).x
      val minY = convexHull.minBy(_.y).y
      val maxY = convexHull.maxBy(_.y).y
      Right(Bounds(Point(minX, minY), Point(maxX, maxY)))
    }
  }

}

/**
 * Represents a collection of points in 2D geometric space.
 *
 * @param points The list of points in the collection.
 */
case class MultiPoint(points: List[Point]) extends Geometry[MultiPoint] {

  /**
   * Translates the MultiPoint by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated MultiPoint.
   */
  override def translate(dx: Double, dy: Double): Either[GeometryError, MultiPoint] =
    points.traverse(_.translate(dx, dy)).map(MultiPoint)

  /**
   * Rotates the MultiPoint by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated MultiPoint.
   */
  override def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, MultiPoint] =
    points.traverse(_.rotate(angle, origin)).map(MultiPoint)

  /**
   * Scales the MultiPoint by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled MultiPoint.
   */
  override def scale(factor: Double): Either[GeometryError, MultiPoint] =
    points.traverse(_.scale(factor)).map(MultiPoint)

  /**
   * Returns the coordinates of the MultiPoint as a list of points.
   *
   * @return Either an error if retrieving the coordinates fails, or the list of coordinates.
   */
  override def coords: Either[GeometryError, List[Point]] =
    Right(points)

  /**
   * Checks if the MultiPoint is valid.
   *
   * @return Either an error if validating the MultiPoint fails, or a boolean indicating validity.
   */
  override def isValid: Either[GeometryError, Boolean] =
    Right(true)

  /**
   * Decomposes the MultiPoint into a list of simpler geometries.
   *
   * @return Either an error if decomposing the MultiPoint fails, or the list of decomposed geometries.
   */
  override def decompose: Either[GeometryError, List[MultiPoint]] =
    Right(List(this))

  /**
   * Simplifies the MultiPoint (no effect).
   *
   * @param tolerance The tolerance parameter for simplification (not used).
   * @return Either an error if simplifying the MultiPoint fails, or the MultiPoint itself.
   */
  override def simplify(tolerance: Double): Either[GeometryError, MultiPoint] =
    Right(this)

  /**
   * Calculates the bounds of the MultiPoint using the convex hull algorithm.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the MultiPoint.
   */
  def bounds: Either[GeometryError, Bounds] = {
    if (points.isEmpty) {
      Left(GeometryError.EmptyGeometry("Cannot calculate bounds of an empty MultiPoint."))
    } else {
      val convexHull = ConvexHull.convexHull(points)
      val minX = convexHull.minBy(_.x).x
      val maxX = convexHull.maxBy(_.x).x
      val minY = convexHull.minBy(_.y).y
      val maxY = convexHull.maxBy(_.y).y
      Right(Bounds(Point(minX, minY), Point(maxX, maxY)))
    }
  }

}

/**
 * Represents a collection of polygons in 2D geometric space.
 *
 * @param polygons The list of polygons in the collection.
 */
case class MultiPolygon(polygons: List[Polygon]) extends Geometry[MultiPolygon] {

  /**
   * Translates the MultiPolygon by the specified distances along the x and y axes.
   *
   * @param dx The distance to translate along the x-axis.
   * @param dy The distance to translate along the y-axis.
   * @return Either an error if the translation fails, or the translated MultiPolygon.
   */
  override def translate(dx: Double, dy: Double): Either[GeometryError, MultiPolygon] =
    polygons.traverse(_.translate(dx, dy)).map(MultiPolygon)

  /**
   * Rotates the MultiPolygon by the specified angle around the given origin point.
   *
   * @param angle  The angle of rotation in degrees.
   * @param origin The origin point for the rotation (default: (0, 0)).
   * @return Either an error if the rotation fails, or the rotated MultiPolygon.
   */
  override def rotate(angle: Double, origin: Point = Point(0, 0)): Either[GeometryError, MultiPolygon] =
    polygons.traverse(_.rotate(angle, origin)).map(MultiPolygon)

  /**
   * Scales the MultiPolygon by the specified factor.
   *
   * @param factor The scaling factor.
   * @return Either an error if the scaling fails, or the scaled MultiPolygon.
   */
  override def scale(factor: Double): Either[GeometryError, MultiPolygon] =
    polygons.traverse(_.scale(factor)).map(MultiPolygon)

  /**
   * Returns the coordinates of the MultiPolygon as a list of polygons' coordinates.
   *
   * @return Either an error if retrieving the coordinates fails, or the list of coordinates.
   */
  override def coords: Either[GeometryError, List[Point]] =
    Right(polygons.flatMap(_.coords.getOrElse(Nil)))

  /**
   * Checks if the MultiPolygon is valid.
   *
   * @return Either an error if validating the MultiPolygon fails, or a boolean indicating validity.
   */
  override def isValid: Either[GeometryError, Boolean] =
    if polygons.forall(_.isValid.getOrElse(false)) then
      Right(true)
    else
      Left(GeometryError.InvalidPolygon("MultiPolygon contains invalid polygons."))

  /**
   * Decomposes the MultiPolygon into a list of simpler geometries.
   *
   * @return Either an error if decomposing the MultiPolygon fails, or the list of decomposed geometries.
   */
  override def decompose: Either[GeometryError, List[MultiPolygon]] =
    Right(List(this))

  /**
   * Simplifies the MultiPolygon by reducing the number of vertices or control points.
   *
   * @param tolerance The tolerance parameter for simplification.
   * @return Either an error if simplifying the MultiPolygon fails, or the simplified MultiPolygon.
   */
  override def simplify(tolerance: Double): Either[GeometryError, MultiPolygon] =
    polygons.traverse(_.simplify(tolerance)).map(MultiPolygon)

  /**
   * Calculates the bounds of the MultiPolygon using the convex hull algorithm.
   *
   * @return Either an error if calculating the bounds fails, or the bounds of the MultiPolygon.
   */
  def bounds: Either[GeometryError, Bounds] = {
    val allPoints = polygons.flatMap(polygon => polygon.exteriorRing.points ++ polygon.interiorRings.flatMap(_.points))
    if (allPoints.isEmpty) {
      Left(GeometryError.InvalidPolygon("Cannot calculate bounds of an empty MultiPolygon."))
    } else {
      val convexHull = ConvexHull.convexHull(allPoints)
      val minX = convexHull.minBy(_.x).x
      val maxX = convexHull.maxBy(_.x).x
      val minY = convexHull.minBy(_.y).y
      val maxY = convexHull.maxBy(_.y).y
      Right(Bounds(Point(minX, minY), Point(maxX, maxY)))
    }
  }

}

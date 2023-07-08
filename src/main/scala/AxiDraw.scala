package com.axidraw

import cats.data.EitherT
import cats.effect.IO

/**
 * Represents the AxiDraw machine model.
 *
 * @param name   the name of the AxiDraw model
 * @param width  the width of the plot area in inches
 * @param height the height of the plot area in inches
 */
enum AxiDrawModel(val name: String, val width: Double, val height: Double):
  case V3 extends AxiDrawModel("AxiDraw V3", 8.5, 11)
  case V3A3 extends AxiDrawModel("AxiDraw V3/A3", 11, 17)
  case SEA3 extends AxiDrawModel("AxiDraw SE/A3", 11, 17)
  case Mini extends AxiDrawModel("MiniKit2", 6, 4)

/**
 * Represents the plot area of the AxiDraw machine.
 *
 * @param width  the width of the plot area in inches
 * @param height the height of the plot area in inches
 */
case class PlotArea(width: Double, height: Double)

/**
 * Represents the AxiDraw machine.
 *
 * @param device   the device to which the AxiDraw is connected
 * @param pen      the pen of the AxiDraw
 * @param motor    the motor of the AxiDraw
 * @param plotArea the plot area of the AxiDraw machine
 */
case class AxiDraw(device: Device, pen: Pen, motor: Motor, plotArea: PlotArea) {

  /**
   * Executes a drawing by moving the pen along a sequence of paths.
   *
   * @param drawing          the drawing to be executed
   * @param progressCallback a function that takes the total amount of paths and the current path number
   *                         and updates the progress
   * @return an `IO` effect representing the result of the drawing execution
   */
  def executeDrawing(drawing: Drawing, progressCallback: (Int, Int) => Unit): IO[Either[AxiDrawError, Unit]] = ???

}

object AxiDraw {

  /**
   * Creates a new instance of AxiDraw for the specified model.
   *
   * @param axiDrawModel the model of the AxiDraw
   * @return the created AxiDraw instance wrapped in the IO effect
   */
  def apply(axiDrawModel: AxiDrawModel): IO[Either[AxiDrawError, AxiDraw]] = ???

}

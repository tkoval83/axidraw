package com.axidraw

import cats.effect.IO

/**
 * Represents the state of the pen.
 *
 * @param isDown   Indicates whether the pen is down (touching the paper) or up.
 * @param position The current position of the pen.
 */
case class PenState(isDown: Boolean, position: Point)

/**
 * Represents a pen used for drawing on the AxiDraw device.
 *
 * The `Pen` class provides methods for controlling the pen of the AxiDraw device,
 * including lowering and raising the pen, toggling the pen state, and querying the current pen state.
 *
 * Each `Pen` instance is associated with a `Device` instance that represents the AxiDraw device,
 * and a `PenState` instance that represents the current state of the pen.
 *
 * @param device The device associated with the pen.
 * @param state  The current state of the pen.
 */
case class Pen(device: Device, state: PenState) {

  /**
   * Lowers the pen to touch the paper.
   *
   * This method first checks if the pen is already down. If it is, no action is taken and the pen state remains unchanged.
   * If the pen is up, it sends the "SP" command to the device to lower the pen and updates the pen state to down.
   *
   * @param duration The duration of the pen movement in milliseconds (optional).
   * @param portBPin The portB pin to use for the output (optional).
   * @return An `IO` that represents the effect of lowering the pen.
   *         If the command is successfully sent, it returns `Right(this)`. Otherwise, it returns `Left` with a specific `PenError` indicating the cause of the error.
   */
  def down(duration: Option[Int] = None, portBPin: Option[Int] = None): IO[Either[PenError, Pen]] = {
    if (state.isDown) {
      IO.pure(Right(this)) // Pen is already down, no action is taken
    } else {
      val command = buildCommand(1, duration, portBPin)
      device.sendCommand(command).attempt.flatMap {
        case Right(_) =>
          val updatedState = state.copy(isDown = true)
          IO.pure(Right(copy(state = updatedState)))
        case Left(error) => IO.pure(Left(PenError.CommandError(error.getMessage)))
      }
    }
  }

  /**
   * Raises the pen from the paper.
   *
   * This method first checks if the pen is already up. If it is, no action is taken and the pen state remains unchanged.
   * If the pen is down, it sends the "SP" command to the device to raise the pen and updates the pen state to up.
   *
   * @param duration The duration of the pen movement in milliseconds (optional).
   * @param portBPin The portB pin to use for the output (optional).
   * @return An `IO` that represents the effect of raising the pen.
   *         If the command is successfully sent, it returns `Right(this)`. Otherwise, it returns `Left` with a specific `PenError` indicating the cause of the error.
   */
  def up(duration: Option[Int] = None, portBPin: Option[Int] = None): IO[Either[PenError, Pen]] = {
    if (!state.isDown) {
      IO.pure(Right(this)) // Pen is already up, no action is taken
    } else {
      val command = buildCommand(0, duration, portBPin)
      device.sendCommand(command).attempt.flatMap {
        case Right(_) =>
          val updatedState = state.copy(isDown = false)
          IO.pure(Right(copy(state = updatedState)))
        case Left(error) => IO.pure(Left(PenError.CommandError(error.getMessage)))
      }
    }
  }

  /**
   * Toggles the state of the pen (up -> down and down -> up).
   *
   * This method sends the "SP" command to the device to toggle the pen state.
   *
   * @param duration The duration of the pen movement in milliseconds (optional).
   * @param portBPin The portB pin to use for the output (optional).
   * @return An `IO` that represents the effect of toggling the pen state.
   *         If the command is successfully sent, it returns `Right(this)`. Otherwise, it returns `Left` with a specific `PenError` indicating the cause of the error.
   */
  def toggle(duration: Option[Int] = None, portBPin: Option[Int] = None): IO[Either[PenError, Pen]] = {
    val value = if (state.isDown) 0 else 1
    val command = buildCommand(value, duration, portBPin)
    device.sendCommand(command).attempt.flatMap {
      case Right(_) =>
        val updatedState = state.copy(isDown = !state.isDown)
        IO.pure(Right(copy(state = updatedState)))
      case Left(error) => IO.pure(Left(PenError.CommandError(error.getMessage)))
    }
  }

  /**
   * Queries the current pen state from the EBB.
   *
   * This method sends the "QP" command to the device to retrieve the current pen state.
   *
   * @return An `IO` effect that represents the result of querying the pen state.
   *         If the command is successfully sent and the pen state is obtained, it returns `Right(this)`.
   *         Otherwise, it returns `Left` with a specific `PenError` indicating the cause of the error.
   */
  def queryState: IO[Either[PenError, Pen]] = {
    device.sendCommand("QP").attempt.flatMap {
      case Right(Right(response)) =>
        val isDown = response.trim.toInt == 0
        val updatedState = state.copy(isDown = isDown)
        IO.pure(Right(copy(state = updatedState)))
      case Left(error) => IO.pure(Left(PenError.CommandError(error.getMessage)))
    }
  }

  /**
   * Builds the command string for the "SP" command with optional arguments.
   *
   * @param value    The value indicating whether to raise or lower the pen.
   * @param duration The duration of the pen movement in milliseconds (optional).
   * @param portBPin The portB pin to use for the output (optional).
   * @return The formatted command string.
   */
  private def buildCommand(value: Int, duration: Option[Int], portBPin: Option[Int]): String = {
    val durationStr = duration.map(d => s",$d").getOrElse("")
    val portBPinStr = portBPin.map(p => s",$p").getOrElse("")
    s"SP,$value$durationStr$portBPinStr;"
  }

}

object Pen {

  /**
   * Creates a new instance of Pen with the initial pen state.
   *
   * @param device The device associated with the pen.
   * @return The created Pen instance.
   */
  def apply(device: Device): IO[Either[PenError, Pen]] = {
    val pen = Pen(device, PenState(isDown = false, Point(0, 0))) // Initial state before querying
    pen.queryState.flatMap {
      case Right(updatedPen) => IO.pure(Right(updatedPen))
      case Left(error) => IO.pure(Left(error))
    }
  }

}

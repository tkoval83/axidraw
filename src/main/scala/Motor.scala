package com.axidraw

import cats.effect.IO

/**
 * Represents the step mode for a motor. Each step mode is associated with a number used in motor commands.
 */
enum StepMode(val value: Int):
  /** Disables motor 1 */
  case MotorDisabled extends StepMode(0)

  /** Enables motor 1, sets global step mode to 1/16 step mode */
  case SixteenthStep extends StepMode(1)

  /** Enables motor 1, sets global step mode to 1/8 step mode */
  case EighthStep extends StepMode(2)

  /** Enables motor 1, sets global step mode to 1/4 step mode */
  case QuarterStep extends StepMode(3)

  /** Enables motor 1, sets global step mode to 1/2 step mode */
  case HalfStep extends StepMode(4)

  /** Enables motor 1, sets global step mode to full step mode */
  case FullStep extends StepMode(5)

/**
 * Represents the state of a motor, including its position and step mode.
 *
 * @param position the current position of the motor
 * @param enabled  indicates whether the motor is enabled or disabled
 */
case class MotorState(position: Int, enabled: Boolean)

/**
 * Represents a motor of the AxiDraw machine.
 *
 * @param device the device to which the motor is connected
 * @param motor1 the state of motor 1
 * @param motor2 the state of motor 2
 */
case class Motor(device: Device, motor1: MotorState, motor2: MotorState) {

  /**
   * Enables or disables the motors with the specified step modes.
   *
   * @param stepMode1 the enable value for motor 1
   * @param stepMode2 the enable value for motor 2
   * @return an `IO` effect that represents the result of enabling or disabling the motors
   */
  def enable(stepMode1: StepMode, stepMode2: StepMode): IO[Either[MotorError, Motor]] = {
    val command = s"EM,${stepMode1.value},${stepMode2.value}"
    device.sendCommand(command).flatMap {
      case Right(_) =>
        val updatedMotor1 = motor1.copy(enabled = stepMode1.value != 0)
        val updatedMotor2 = motor2.copy(enabled = stepMode2.value != 0)
        val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
        IO.pure(Right(updatedMotor))
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.message)))
    }
  }

  /**
   * Disables the motors.
   *
   * @return an `IO` effect that represents the result of disabling the motors
   */
  def disable(): IO[Either[MotorError, Motor]] = enable(StepMode.MotorDisabled, StepMode.MotorDisabled)

  /**
   * Clears the global step positions of the motors.
   *
   * @return an `IO` effect that represents the result of clearing the step positions
   */
  def clearStepPositions: IO[Either[MotorError, Motor]] =
    device.sendCommand("CS").attempt.flatMap {
      case Right(_) =>
        val updatedMotor1 = motor1.copy(position = 0)
        val updatedMotor2 = motor2.copy(position = 0)
        val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
        IO.pure(Right(this))
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.getMessage)))
    }

  /**
   * Moves the motors to the specified positions with the given duration.
   *
   * @param position1 the position for motor 1
   * @param position2 the position for motor 2
   * @return a function that takes the duration and returns an `IO` effect representing the result of moving the motors
   */
  def move(position1: Int, position2: Int): Int => IO[Either[MotorError, Motor]] = { duration =>
    val command = s"SM,$duration,${position1},${position2}"
    device.sendCommand(command).flatMap {
      case Right(_) =>
        val updatedMotor1 = motor1.copy(position = position1)
        val updatedMotor2 = motor2.copy(position = position2)
        val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
        IO.pure(Right(updatedMotor))
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.message)))
    }
  }

  /**
   * Moves the motors to the specified positions using the XM command with the given duration.
   *
   * @param axisStepsA the steps for Axis A
   * @param axisStepsB the steps for Axis B
   * @return a function that takes the duration and returns an `IO` effect representing the result of moving the motors
   */
  def moveMixed(axisStepsA: Int, axisStepsB: Int): Int => IO[Either[MotorError, Motor]] = { duration =>
    val command = s"XM,$duration,${axisStepsA},${axisStepsB}"
    device.sendCommand(command).flatMap {
      case Right(_) =>
        // Here we update positions as the sum and difference of the steps for mixed-axis geometry
        val updatedPosition1 = motor1.position + axisStepsA + axisStepsB
        val updatedPosition2 = motor2.position + axisStepsA - axisStepsB
        val updatedMotor1 = motor1.copy(position = updatedPosition1)
        val updatedMotor2 = motor2.copy(position = updatedPosition2)
        val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
        IO.pure(Right(updatedMotor))
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.message)))
    }
  }

  /**
   * Queries the current motor status from the EBB.
   *
   * This method sends the "QM" command to the device to retrieve the current motor status.
   *
   * @return An `IO` effect that represents the result of querying the motor status.
   *         If the command is successfully sent and the motor status is obtained, it returns `Right(this)`.
   *         Otherwise, it returns `Left` with a specific `MotorError` indicating the cause of the error.
   */
  def queryStatus: IO[Either[MotorError, Motor]] =
    device.sendCommand("QM").attempt.flatMap {
      case Right(Right(response)) =>
        val responseParts = response.split(",")
        if (responseParts.length >= 5) {
          val commandStatus = responseParts(1).toInt
          val motor1Status = responseParts(2).toInt
          val motor2Status = responseParts(3).toInt
          val fifoStatus = responseParts(4).toInt
          val updatedMotor1 = motor1.copy(enabled = motor1Status == 1)
          val updatedMotor2 = motor2.copy(enabled = motor2Status == 1)
          val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
          IO.pure(Right(updatedMotor))
        } else {
          IO.pure(Left(MotorError.CommandError("Invalid response")))
        }
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.getMessage)))
    }

  /**
   * Queries the current step positions of the motors from the EBB.
   *
   * This method sends the "QS" command to the device to retrieve the current step positions.
   *
   * @return An `IO` effect that represents the result of querying the step positions.
   *         If the command is successfully sent and the step positions are obtained, it returns `Right(this)`.
   *         Otherwise, it returns `Left` with a specific `MotorError` indicating the cause of the error.
   */
  def queryStepPositions: IO[Either[MotorError, Motor]] =
    device.sendCommand("QS").attempt.flatMap {
      case Right(Right(response)) =>
        val responseParts = response.split(",")
        if (responseParts.length >= 2) {
          val motor1StepPosition = responseParts(0).toInt
          val motor2StepPosition = responseParts(1).toInt
          val updatedMotor1 = motor1.copy(position = motor1StepPosition)
          val updatedMotor2 = motor2.copy(position = motor2StepPosition)
          val updatedMotor = this.copy(motor1 = updatedMotor1, motor2 = updatedMotor2)
          IO.pure(Right(updatedMotor))
        } else {
          IO.pure(Left(MotorError.CommandError("Invalid response")))
        }
      case Left(error) => IO.pure(Left(MotorError.CommandError(error.getMessage)))
    }
}

object Motor {

  /**
   * Creates a new instance of Motor with the specified device and initial motor states.
   * If the `motor1` and `motor2` states are not provided, they will be initialized by querying the current motor status.
   *
   * @param device the device to which the motor is connected
   * @return a new `Motor` instance
   */
  def apply(device: Device): IO[Either[MotorError, Motor]] = {
    val motor = Motor(device, MotorState(0, enabled = false), MotorState(0, enabled = false))
    for {
      motorStatus <- motor.queryStatus
      motorPositions <- motor.queryStepPositions
    } yield for {
      updatedMotorStatus <- motorStatus
      updatedMotorPositions <- motorPositions
    } yield updatedMotorPositions.copy(motor1 = updatedMotorStatus.motor1, motor2 = updatedMotorStatus.motor2)
  }

}

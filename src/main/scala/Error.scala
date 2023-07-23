package com.axidraw

sealed trait Error extends Product with Serializable {

  /**
   * Gets the error message associated with the error.
   *
   * @return The error message.
   */
  def message: String
}

/**
 * Represents the possible errors related to geometry operations.
 */
enum GeometryError extends Error {

  /**
   * Error indicating an empty geometry.
   *
   * @param message The error message.
   */
  case EmptyGeometry(message: String)

  /**
   * Error indicating an invalid line string.
   *
   * @param message The error message.
   */
  case InvalidLineString(message: String)

  /**
   * Error indicating an invalid polygon.
   *
   * @param message The error message.
   */
  case InvalidPolygon(message: String)

  /**
   * Error indicating a scaling error.
   *
   * @param message The error message.
   */
  case ScalingError(message: String)

  /**
   * Error indicating a rotation error.
   *
   * @param message The error message.
   */
  case RotationError(message: String)

  /**
   * Error indicating a translation error.
   *
   * @param message The error message.
   */
  case TranslationError(message: String)
}

/**
 * Represents the possible errors related to the device (AxiDraw plotter) operations.
 */
enum DeviceError extends Error {

  /**
   * Error indicating a connection error with the device.
   *
   * @param message The error message.
   */
  case ConnectionError(message: String)

  /**
   * Error indicating a command error with the device.
   *
   * @param message The error message.
   */
  case CommandError(message: String)

  /**
   * Error indicating a read error from the device.
   *
   * @param message The error message.
   */
  case ReadError(message: String)

  /**
   * Error indicating a write error to the device.
   *
   * @param message The error message.
   */
  case WriteError(message: String)
}

/**
 * Represents the possible errors related to the pen operations.
 */
enum PenError extends Error {

  /**
   * Error indicating a command error with the pen.
   *
   * @param message The error message.
   */
  case CommandError(message: String)

  /**
   * Error indicating a move error of the pen.
   *
   * @param message The error message.
   */
  case MoveError(message: String)
}

/**
 * Represents the possible errors related to the motor operations.
 */
enum MotorError extends Error {

  /**
   * Error indicating a command error with the motor.
   *
   * @param message The error message.
   */
  case CommandError(message: String)
}

/**
 * Represents the possible errors related to the AxiDraw plotter execution.
 */
enum AxiDrawError extends Error {

  /**
   * Error indicating an execution error of the AxiDraw plotter.
   *
   * @param message The error message.
   */
  case ExecutionError(message: String)
}

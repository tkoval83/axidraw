package com.axidraw

sealed trait Error extends Product with Serializable {
  def message: String
}

enum GeometryError extends Error {
  case InvalidLineString(message: String)
  case InvalidPolygon(message: String)
  case ScalingError(message: String)
  case RotationError(message: String)
  case TranslationError(message: String)
}

enum DeviceError extends Error {
  case ConnectionError(message: String)
  case CommandError(message: String)
  case ReadError(message: String)
  case WriteError(message: String)
}

enum PenError extends Error {
  case CommandError(message: String)
  case MoveError(message: String)
}

enum MotorError extends Error {
  case CommandError(message: String)
}

enum AxiDrawError extends Error {
  case ExecutionError(message: String)
}



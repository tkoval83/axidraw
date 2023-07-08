package com.axidraw

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.syntax.all.*
import com.fazecast.jSerialComm.{SerialPort, SerialPortDataListener, SerialPortEvent}

sealed trait DeviceState

case object Connected extends DeviceState

case object Disconnected extends DeviceState

/**
 * The `Device` class represents a device connected to a serial port.
 *
 * @param portDescriptor The descriptor of the serial port associated with the device.
 * @param state          The current state of the device, which can be either `Connected` or `Disconnected`.
 * @param serialPort     The active `SerialPort` instance associated with the device.
 */
case class Device(portDescriptor: String, state: DeviceState = Disconnected, serialPort: Option[SerialPort] = None) {

  /**
   * Connects to the device.
   *
   * @return An `IO[Either[DeviceError, Device]]` that represents the effect of connecting to the device.
   *         If the connection is successful, it returns `Right(Device)`. Otherwise, it returns `Left` with an error message.
   */
  def connect(ibaudRate: Int = 9600, parity: Int = SerialPort.NO_PARITY, numStopBits: Int = SerialPort.ONE_STOP_BIT,
              numDataBits: Int = 8): IO[Either[DeviceError, Device]] = IO {
    def connectDevice() = {
      val newSerialPort = SerialPort.getCommPort(portDescriptor)
      newSerialPort.setComPortParameters(ibaudRate, numDataBits, numStopBits, parity)
      newSerialPort.setComPortTimeouts(SerialPort.TIMEOUT_READ_SEMI_BLOCKING, 0, 0)
      if (newSerialPort.openPort()) {
        newSerialPort.addDataListener(SerialPortListener)
        println("Connected to port: " + portDescriptor)
        Right(copy(state = Connected, serialPort = Some(newSerialPort)))
      } else {
        println("Failed to open port: " + portDescriptor)
        Left(DeviceError.ConnectionError("Failed to open port"))
      }
    }

    state match {
      case Connected =>
        println("Already connected")
        Right(this)
      case Disconnected => connectDevice()
    }
  }.handleErrorWith { error =>
    println("Error during connection: " + error.getMessage)
    IO.pure(Left(DeviceError.ConnectionError(error.getMessage)))
  }

  /**
   * Disconnects from the device.
   *
   * @return An `IO[Either[DeviceError, Device]]` that represents the effect of disconnecting from the device.
   *         If the disconnection is successful or the device is already disconnected, it returns `Right(Device)`.
   *         Otherwise, it returns `Left` with a specific `DeviceError` indicating the cause of the error.
   */
  def disconnect: IO[Either[DeviceError, Device]] = IO {
    state match {
      case Disconnected =>
        Right(this)
      case Connected =>
        serialPort.fold[Either[DeviceError, Unit]](Left(DeviceError.ConnectionError("SerialPort instance not available"))) { port =>
          if (port.isOpen) {
            port.removeDataListener()
            port.closePort()
            println("Disconnected from port: " + portDescriptor)
            Right(())
          } else {
            println("Port is already closed: " + portDescriptor)
            Right(()) // Port already closed, return Right
          }
        }.map { _ =>
          copy(state = Disconnected, serialPort = None)
        }
    }
  }.handleErrorWith { error =>
    println("Error during disconnection: " + error.getMessage)
    IO.pure(Left(DeviceError.ConnectionError(error.getMessage)))
  }

  /**
   * Sends a command to the device and reads the response.
   *
   * @param command The command to send to the device.
   * @return An `IO[Either[DeviceError, String]]` that represents the effect of sending a command to the device and reading the response.
   *         If the command is successfully sent and a response is received, it returns `Right(response)`.
   *         Otherwise, it returns `Left` with a specific `DeviceError` indicating the cause of the error.
   */
  def sendCommand(command: String): IO[Either[DeviceError, String]] = {
    state match {
      case Disconnected =>
        IO.pure(Left(DeviceError.ConnectionError("Cannot send command, port is not open")))
      case Connected =>
        serialPort match {
          case None =>
            IO.pure(Left(DeviceError.ConnectionError("Failed to send command")))
          case Some(port) =>
            writeCommand(port, command)
              .flatMap(_ => readResponse(port))
              .map(checkResponse)
        }
    }
  }

  /**
   * Writes a command to the device.
   *
   * @param port    The serial port to write to.
   * @param command The command to write.
   * @return An `IO[Unit]` representing the effect of writing the command. If the write is successful,
   *         it returns an `IO` with unit. If an error occurs, the error is raised within the `IO`.
   */
  private def writeCommand(port: SerialPort, command: String): IO[Unit] = {
    val acquireOutputStream = Resource.fromAutoCloseable(IO(port.getOutputStream))
    acquireOutputStream.use { outputStream =>
      val ebbCommand = command + "\r"
      IO.blocking(outputStream.write(ebbCommand.getBytes))
        .flatMap(_ => IO.blocking(outputStream.flush()))
    }.handleErrorWith { error =>
      println("Error writing command: " + error.getMessage)
      IO.raiseError(error)
    }
  }

  /**
   * Reads the response from the device.
   *
   * @param port The serial port to read from.
   * @return An `IO[String]` representing the effect of reading the response. If the read is successful,
   *         it returns an `IO` with the response as a string. If an error occurs, the error is raised within the `IO`.
   */
  private def readResponse(port: SerialPort): IO[String] = {
    val acquireInputStream = Resource.fromAutoCloseable(IO(port.getInputStream))
    acquireInputStream.use { inputStream =>
      IO.blocking {
        val response = new Array[Byte](1024)
        val bytesRead = inputStream.read(response)
        val responseStr = new String(response, 0, bytesRead).trim
        println(s"Response: $responseStr")
        responseStr
      }
    }.handleErrorWith { error =>
      println("Error reading response: " + error.getMessage)
      IO.raiseError(error)
    }
  }

  /**
   * Checks the response received from the device.
   *
   * @param response The response to check.
   * @return An `Either[DeviceError, String]` that contains the original response if it's valid,
   *         or a `DeviceError` if the response is not valid.
   */
  private def checkResponse(response: String): Either[DeviceError, String] = {
    if (response.endsWith("\r")) Right(response)
    else Left(DeviceError.CommandError(response))
  }

  /**
   * Custom data listener for handling port disconnected events.
   */
  private object SerialPortListener extends SerialPortDataListener {
    override def getListeningEvents: Int = SerialPort.LISTENING_EVENT_PORT_DISCONNECTED

    override def serialEvent(event: SerialPortEvent): Unit = {
      if (event.getEventType == SerialPort.LISTENING_EVENT_PORT_DISCONNECTED) {
        disconnect.unsafeRunSync()
      }
    }
  }

}

object Device {

  /**
   * Creates a new instance of Device with an available /dev/ttyACM serial port.
   *
   * @return An `IO[Either[DeviceError, Device]]` representing the device instance, or `Left` with a specific `DeviceError`
   *         indicating the cause of the error if no port is available.
   */
  def apply(): IO[Either[DeviceError, Device]] = {
    findAvailablePort().map(_.map(portDescriptor => Device(portDescriptor, Disconnected, None)))
  }

  /**
   * Finds an available /dev/ttyACM serial port.
   *
   * @return An `IO[Either[DeviceError, String]]` representing the effect of finding an available port.
   *         If a port is found, it returns `Right(portDescriptor)`. Otherwise, it returns `Left` with a specific `DeviceError`
   *         indicating that no available port was found.
   */
  private def findAvailablePort(): IO[Either[DeviceError, String]] = IO {
    val ports = SerialPort.getCommPorts
    val portOpt = ports.find(_.getSystemPortName.startsWith("/dev/ttyACM")).map(_.getSystemPortName)
    portOpt.toRight(DeviceError.ConnectionError("No available /dev/ttyACM port found"))
  }
}

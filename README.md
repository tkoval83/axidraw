# AxiDraw Library

The AxiDraw Library provides a set of abstractions and utilities for interacting with the AxiDraw machine, a pen plotter manufactured by Evil Mad Scientist. The library is written in Scala and leverages the Cats Effect library for concurrency and functional programming.

## Features

- Connect and disconnect from the AxiDraw machine via serial port communication.
- Control the pen state, including raising and lowering the pen.
- Control the motor state, including enabling and disabling the motors.
- Execute drawings by moving the pen along paths.
- Query the status and step positions of the motors.
- Support for different models of the AxiDraw machine.

## Requirements

- Scala 3.x
- Cats Effect 3.x

## Getting Started

To use the AxiDraw Library in your project, add the following dependency to your build file:

```scala
libraryDependencies += "com.axidraw" %% "axidraw-library" % "1.0.0"
```

## Usage
Here's an example of how to use the AxiDraw Library to connect to the AxiDraw machine, execute a drawing, and display a progress bar:

```scala
import cats.effect.{IO, IOApp}
import com.axidraw._

object Main extends IOApp.Simple {

  def run: IO[Unit] =
    AxiDraw(AxiDrawModel.V3).flatMap {
      case Right(axiDraw) =>
        val drawing = // create your drawing here
        val totalPaths = drawing.paths.length
        val progress = new Progress.Bar(totalPaths)

        def updateProgress(currentPath: Int): IO[Unit] =
          progress.increment(1) >> progress.render.flatMap { progressString =>
            IO(print(s"\r$progressString")) >> IO(System.out.flush())
          }

        def handleResult(result: Either[AxiDrawError, Unit]): IO[Unit] =
          result.fold(
            error => progress.stop >> IO(println(s"\nDrawing failed: $error")),
            _ => progress.done >> IO(println("\nDrawing complete!"))
          )

        axiDraw.executeDrawing(drawing, updateProgress).flatMap(handleResult)

      case Left(error) =>
        IO(println(s"Failed to initialize AxiDraw: $error"))
    }
}
```

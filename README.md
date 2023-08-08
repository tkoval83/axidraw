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

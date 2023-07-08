package com.axidraw

import cats.effect.*
import cats.effect.unsafe.implicits.global
import cats.implicits.*

import java.time.Duration
import scala.concurrent.duration.*

/**
 * A console progress bar implementation.
 */
object Progress {

  /**
   * Formats a `Duration` object into a pretty time string.
   *
   * @param duration the duration to format
   * @return the formatted time string
   */
  def prettyTime(duration: Duration): String = {
    val totalSeconds = duration.toSeconds
    val hours = totalSeconds / 3600
    val minutes = (totalSeconds % 3600) / 60
    val seconds = totalSeconds % 60
    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  /**
   * Represents a progress bar.
   *
   * @param maxValue the maximum value of the progress bar
   * @param minValue the minimum value of the progress bar (default: 0)
   * @param enabled  whether the progress bar is enabled (default: true)
   * @param clock    the clock for time-related operations
   */
  class Bar(maxValue: Double, minValue: Double = 0, enabled: Boolean = true)(implicit clock: Clock[IO]) {
    private val startTime = clock.realTime
    private var value = minValue

    /**
     * Calculates the percentage of completion of the progress bar.
     *
     * @return the percentage of completion
     */
    def percentComplete: Double = 100.0 * (value - minValue) / (maxValue - minValue)

    /**
     * Retrieves the elapsed time since the progress bar started.
     *
     * @return the elapsed time
     */
    def elapsedTime: IO[FiniteDuration] =
      startTime.flatMap { t =>
        clock.realTime.map { current =>
          FiniteDuration((current - t).toMillis, MILLISECONDS)
        }
      }

    /**
     * Calculates the estimated time of completion.
     *
     * @return the estimated time of completion
     */
    def eta: IO[Duration] = for {
      elapsed <- elapsedTime
      percent <- IO(percentComplete)
    } yield {
      if (percent == 0) Duration.ZERO
      else Duration.ofMillis(((1 - percent / 100.0) * elapsed.toMillis / (percent / 100.0)).toLong)
    }

    /**
     * Increments the progress bar by the specified delta.
     *
     * @param delta the delta to increment by
     * @return an `IO` action representing the increment operation
     */
    def increment(delta: Double): IO[Unit] = IO {
      value += delta
      update(value)
    }

    /**
     * Updates the progress bar to the specified value.
     *
     * @param newValue the new value of the progress bar
     * @return an `IO` action representing the update operation
     */
    def update(newValue: Double): IO[Unit] = IO {
      value = newValue
      if (enabled) {
        val text = render.unsafeRunSync()
        print(s"\r$text")
        System.out.flush()
      }
    }

    /**
     * Marks the progress bar as done.
     *
     * @return an `IO` action representing the completion of the progress bar
     */
    def done: IO[Unit] = update(maxValue) >> stop

    /**
     * Stops the progress bar and prints a newline character.
     *
     * @return an `IO` action representing the stopping of the progress bar
     */
    def stop: IO[Unit] = IO {
      println()
      System.out.flush()
    }

    /**
     * Renders the progress bar as a string.
     *
     * @return an `IO` action representing the rendering of the progress bar
     */
    def render: IO[String] = for {
      percent <- IO(f"$percentComplete%3.0f%%")
      value <- IO(if (minValue == 0) f"($value%.0f of $maxValue%.0f)" else f"($value%.0f)")
      bar <- renderBar()
      elapsed <- elapsedTime.map(d => prettyTime(Duration.ofSeconds(d.toSeconds)))
      etaVal <- eta.map(d => prettyTime(Duration.ofSeconds(d.toSeconds)))
    } yield s"$percent $value $bar $elapsed $etaVal".trim

    /**
     * Renders the progress bar as a bar string.
     *
     * @param size the size of the progress bar (default: 30)
     * @return an `IO` action representing the rendering of the progress bar
     */
    def renderBar(size: Int = 30): IO[String] = IO {
      val a = (percentComplete / 100.0 * size).round.toInt
      val b = size - a
      s"[${"#" * a}${"-" * b}]"
    }
  }
}

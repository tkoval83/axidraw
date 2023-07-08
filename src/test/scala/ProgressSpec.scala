package com.axidraw

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.axidraw.Progress
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class ProgressSpec extends AnyFlatSpec with Matchers {

  "Progress" should "increment and render progress bar correctly" in {
    val bar = new Progress.Bar(100)
    val updates = (1 to 100).map(_ => bar.increment(1))

    updates.foreach(_.unsafeRunSync())
    val progressBar = bar.render.unsafeRunSync()

    progressBar should fullyMatch regex """\d+% \(\d+ of \d+\) \[#+\] \d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}"""
  }

  it should "render a completed progress bar correctly" in {
    val bar = new Progress.Bar(100)
    bar.done.unsafeRunSync()

    val progressBar = bar.render.unsafeRunSync()

    progressBar should fullyMatch regex """\d+% \(\d+ of \d+\) \[#+\] \d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}"""
  }

  it should "render a progress bar with custom min and max values correctly" in {
    val bar = new Progress.Bar(200, 50)
    val updates = (50 to 150).map(_ => bar.increment(1))

    updates.foreach(_.unsafeRunSync())
    val progressBar = bar.render.unsafeRunSync()

    progressBar should fullyMatch regex """\d+% \(\d+\) \[[#|-]+\] \d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}"""
  }

  it should "render a progress bar with disabled output correctly" in {
    val bar = new Progress.Bar(100, enabled = false)
    val progressBar = bar.render.unsafeRunSync()

    progressBar should fullyMatch regex """\d+% \(\d+ of \d+\) \[\-+\] \d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}"""
  }

  import cats.syntax.traverse._
  import cats.syntax.foldable._

  it should "render a progress bar with elapsed time and ETA correctly" in {
    val bar = new Progress.Bar(100)
    val updates = (1 to 50).map { i =>
      val update = bar.increment(2)
      if (i == 25) update *> IO.sleep(500.milliseconds) // Simulate longer processing time with IO.sleep
      else update
    }

    val updateSequence = updates.toList.traverse_(_.void) // Convert updates to a List and use traverse_
    updateSequence.unsafeRunSync() // Run the updates one after another
    val progressBar = bar.render.unsafeRunSync()

    progressBar should fullyMatch regex """\d+% \(\d+ of \d+\) \[#+\] \d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}"""
  }

}

package com.umatrangolo.repl.microbenchmark

import scala.collection.LinearSeq

object Logger { // TODO replace with Console
  def log(msg: String) { println("[%s] -- %s ".format(new java.util.Date, msg)) }
}

case class Timing(time: Long) {
  require(time > 0, "time must be >= 0")

  private[microbenchmark] val ms: Long = TimeUnit.Milliseconds(time)
  private[microbenchmark] val s: Long = TimeUnit.Seconds(time)
  private[microbenchmark] val ns: Long = time  // default time unit is nanosecs

  override def toString = "%d".format(time)
}

case class Outcome(name: Option[String] = None, times: LinearSeq[Long]) {
  lazy val avg = Timing(java.lang.Math.round(times.foldLeft(0d) { (a: Double, o: Long) => a + o } / times.size))
  lazy val max = Timing(times.map { o => o } max)
  lazy val min = Timing(times.map { o => o } min)
  lazy val all = times.map { Timing(_) }
}

object TimeUnit extends Enumeration {
  import java.util.concurrent.{ TimeUnit => JTimeUnit }

  case class TimeUnitValue(sid: String, conv: (Long) => Long) extends Val {
    def apply(ns: Long): Long = conv(ns)
  }

  type TimeUnit = TimeUnitValue // path dependant type --> Scala way of impl 'enumeration'

  val Seconds = new TimeUnitValue("s", JTimeUnit.SECONDS.convert(_, JTimeUnit.NANOSECONDS))
  val Milliseconds = new TimeUnitValue("ms" ,JTimeUnit.MILLISECONDS.convert(_, JTimeUnit.NANOSECONDS))
  val Nanoseconds = new TimeUnitValue("ns", { ns => ns })
}

import TimeUnit._

case class Summary(outcome: Outcome, timeUnit: TimeUnit = TimeUnit.Nanoseconds) {

  def timeUnit(unit: TimeUnit): Summary = this.copy(timeUnit = unit)

  private def tabulate: String =
    """
     | Test: %s
     | ------------------------------
     | Avg time: %s %s
     | Max time: %s %s
     | Min time: %s %s
    """.format(
      outcome.name.getOrElse("?"),
      timeUnit(outcome.avg.ns), timeUnit.sid,
      timeUnit(outcome.max.ns), timeUnit.sid,
      timeUnit(outcome.min.ns), timeUnit.sid
    ).stripMargin

  override def toString = tabulate
}

object Timer {

  private[microbenchmark] def time(n: Int, name: Option[String])(f: => Unit): Outcome = {
    Logger.log("Starting %s ...".format(name.getOrElse("")))

    val times: LinearSeq[Long] = (for {
      i <- 0 until n
    } yield {
      val now = System.nanoTime
      f
      System.nanoTime - now
    }).toList

    Logger.log("Done.")

    Outcome(name, times)
  }

  def time(n: Int)(f: => Unit): Outcome = time(n, None) { f }
  def time(n: Int, name: String)(f: => Unit): Outcome = time(n, Some(name)) { f }
}

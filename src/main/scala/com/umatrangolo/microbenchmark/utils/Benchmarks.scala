package com.umatrangolo.repl.microbenchmark 

/** Simple logger for the REPL. */
object Logger { // TODO replace with Console
  def log(msg: String) { println("[%s] -- %s ".format(new java.util.Date, msg)) }
}

/** This encapsulates the timings of a test run.
  *
  * @param name identifier of this test outcome
  * @param time an array of ''Long'' with the times taken by each run of the test.
  */
case class Outcome(name: Option[String] = None, var times: List[Long]) {
  
  /** Represents a single time outcome of a test run
    *
    * @param time a ''Long'' with the time in ns (default time unit)
    */
  case class Timing(time: Long) {
    private[microbenchmark] val ms: Long = TimeUnit.Milliseconds(time)
    private[microbenchmark] val s: Long = TimeUnit.Seconds(time)
    private[microbenchmark] val ns: Long = time  // default time unit is nanosecs

    override def toString = "%d".format(time)
  }

  val avg = Timing(java.lang.Math.round(times.foldLeft(0d) { (a: Double, o: Long) => a + o } / times.size))
  val max = Timing(times map { o => o } max)
  val min = Timing(times map { o => o } min)
  val all = times map { Timing(_) }  
}

/** Time units supported by the benchmark utils.
  *
  * All the test runs are measured using the ns time unit. Each TimeUnit instance
  * defines an '''apply()''' method to convert from ns to one of the other supported
  * units.
  *
  * e.g.: TimeUnit.Seconds(123456789l) will convert the given ns in s.
  */ 
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
    """.format(outcome.name.getOrElse("?"), 
	       timeUnit(outcome.avg.ns), timeUnit.sid, 
	       timeUnit(outcome.max.ns), timeUnit.sid,
	       timeUnit(outcome.min.ns), timeUnit.sid).stripMargin

  override def toString = tabulate
}

/** Main object that provides the time() method to time benchmark a series of n executions
  * of a given function.
  */ 
object Timer {

  /** Main method that performs the timing of the method.
    *
    * @param n number of times the tracked method should be invoked.
    * @param f function being tracked
    * @return a `Report` describing the observed behavior
    */  
  private[microbenchmark] def time(n: Int, name: Option[String])(f: => Unit): Outcome = {
    Logger.log("Starting %s ...".format(name.getOrElse("")))
    
    val times: List[Long] = List() ++ (0 until n) map { i =>
      val now = System.nanoTime
      f  // timed code TODO: need instrumentation like Google's Caliper ?
      System.nanoTime - now
    }

    Logger.log("Done.")
    
    Outcome(name, times)
  }

  def time(n: Int)(f: => Unit): Outcome = time(n, None) { f } 
  def time(n: Int, name: String)(f: => Unit): Outcome = time(n, Some(name)) { f } 
}

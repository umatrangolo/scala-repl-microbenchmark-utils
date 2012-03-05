package com.umatrangolo.repl.microbenchmark 

/** Simple logger for the REPL. */
object Logger {
  def log(msg: String) { println("[%s] -- %s ".format(new java.util.Date, msg)) }
}

/** This encapsulates the timings of a test run. */
case class Report(name: Option[String] = None, var times: List[Long]) {
  
  case class Timing(time: Long) {
    import java.util.concurrent.TimeUnit

    val inMs: Long = TimeUnit.MILLISECONDS.convert(time, TimeUnit.NANOSECONDS)
    val inSecs: Long = TimeUnit.SECONDS.convert(time, TimeUnit.NANOSECONDS)
    val inNs: Long = time  // default time unit is nanosecs

    override def toString = "%d".format(time)
  }

  val avg = Timing(java.lang.Math.round(times.foldLeft(0d) { (a: Double, o: Long) => a + o } / times.size))
  val max = Timing(times map { o => o } max)
  val min = Timing(times map { o => o } min)
  val all = times map { Timing(_) }  

  private def tabulate(avg: Long, max: Long, min: Long, unit: String): String = 
    """
     | Test: %s
     | ------------------------------
     | Avg time: %s %s
     | Max time: %s %s
     | Min time: %s %s
    """.format(name.getOrElse("*"), avg, unit, max, unit, min, unit).stripMargin

  override def toString = tabulate(avg.inNs, max.inNs, min.inNs, "ns.")
  def inSecs = tabulate(avg.inSecs, max.inSecs, min.inSecs, "s.")
  def inMs = tabulate(avg.inMs, max.inMs, min.inMs, "ms.")
}

/**  Main object that provides the time() method to time a series of n executions
  *  of the code that we are micro benchmarking.
  */ 
object Timer {

  /** Main method that performs the timing of the method.
    *
    * @param n number of times the tracked method should be invoked.
    * @param f function being tracked
    * @return a `Report` describing the observed behavior
    */  
  private[microbenchmark] def time(n: Int, name: Option[String])(f: => Unit): Report = {
    Logger.log("Starting %s ...".format(name.getOrElse("")))
    
    val times: List[Long] = List() ++ (0 until n) map { i =>
      val now = System.nanoTime
      f  // timed code TODO: need instrumentation like Google's Caliper ?
      System.nanoTime - now
    }

    Logger.log("Done.")
    
    Report(name, times)
  }

  def time(n: Int)(f: => Unit): Report = time(n, None) { f } 
  def time(n: Int, name: String)(f: => Unit): Report = time(n, Some(name)) { f } 
}

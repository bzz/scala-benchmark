import com.google.caliper.{Benchmark => CaliperBenchmark}

trait ScalaBenchmark extends CaliperBenchmark {
  
  // helper method to keep the actual benchmarking methods a bit cleaner
  // your code snippet should always return a value that cannot be "optimized away"
  def repeat[@specialized A](reps: Int)(snippet: => A) = {
    // init w/ default value in a fully generic way
    val zero = 0.asInstanceOf[A] 
    var i = 0
    var result = zero
    while (i < reps) {
      val res = snippet 
      // make result depend on the benchmarking snippet result 
      if (res != zero) result = res 
      i = i + 1
    }
    result
  }

}


import annotation.tailrec
import com.google.caliper.Param

class AccountByUrlBenchmark extends ScalaBenchmark {
  
  // to make your benchmark depend on one or more values, create fields with the name you want
  // the parameter to be known by, and add this annotation (see @Param javadocs for more details)
  // caliper will inject the respective value at runtime and make sure to run all combinations 
  @Param(Array("10", "100", "1000", "10000"))
  val length: Int = 0
  
  var array: Array[Int] = _
  
  override def setUp() {
    array = new Array(length)
  }
  


  def timeReturnInLambda(reps: Int) = repeat(reps) { 
    // http://twitter.github.io/effectivescala/#Control structures-Returns
  def _getAccountAndDomainByUrl(accounts: Seq[(String, Array[(String, Regex)])], url: String): (String, String) = {
    accounts foreach { case (account, patterns) =>
      patterns foreach { case (domainName, pattern) =>
        pattern.findFirstIn(url) foreach { matched =>
          return (account, domainName)
        }
      }
    }
    return ("unk","unk")
  }

    var result = 0 
    result // always have your snippet return a value that cannot easily be "optimized away"
  }
  
  // a second benchmarking code snippet
  def timeTFor(reps: Int) = repeat(reps) {
    var result = 0
    tfor(0)(_ < array.length, _ + 1) { i =>
      result += array(i)
    }
    result
в  }
  
  // and a third benchmarking code snippet
  def timeWhile(reps: Int) = repeat(reps) {
    var result = 0
    var i = 0
    while (i < array.length) {
      result += array(i)
      i = i + 1 
    }
    result
  }

  // this is a scala version of Javas "for" loop,
  // we test it against the array.foreach and a plain "while" loop
  @tailrec
  final def tfor[@specialized T](i: T)(test: T => Boolean, inc: T => T)(f: T => Unit) {
    if(test(i)) {
      f(i)
      tfor(inc(i))(test, inc)(f)
    }
  }
  
  override def tearDown() {
    // clean up after yourself if required
  }
  
}

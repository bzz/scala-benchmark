
import annotation.tailrec
import com.google.caliper.Param


//using spark build-in json4s-jackson
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.JsonDSL._

class Benchmark extends ScalaBenchmark {
  
  // to make your benchmark depend on one or more values, create fields with the name you want
  // the parameter to be known by, and add this annotation (see @Param javadocs for more details)
  // caliper will inject the respective value at runtime and make sure to run all combinations 
  @Param(Array("10", "100", "1000"))
  val length: Int = 0
  
  var array: Array[Int] = _
  
  override def setUp() {
    // set up all your benchmark data here
    array = new Array(length)
  }
  
  // the actual code you'd like to test needs to live in one or more methods
  // whose names begin with 'time' and which accept a single 'reps: Int' parameter
  // the body of the method simply executes the code we wish to measure, 'reps' times
  // you can use the 'repeat' from the SimpleBenchmark to repeat with relatively low overhead
  // however, if your code  is fast you want to implement the reps loop directly with 'while'
  def timeScalaNative(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////
    
    val jsonStr = Source.fromURL("http://json-urls").mkString
    val newJson = parse(jsonStr) flatMap { json =>
      implicit lazy val formats = org.json4s.DefaultFormats
      val df = new java.text.SimpleDateFormat("yyyy-MM")
      val datum = (json \ "data").extract[Seq[Map[String, org.json4s.JsonAST.JValue]]]
      datum map { data =>                 //for (each .data[] in json)
        var new_data = immutable.Map(data.toSeq: _*)//does not work with MUTABLE!!!!
        new_data = new_data - "_id"                 //  remove _id, add date
        new_data = new_data updated ("date", df.format(ad_stat("end_timestamp").extract[Long]*1000))
        new_data = new_data updated ("end_timestamp", ad_stat("end_timestamp").extract[Long]*1000)
        new_data = new_data updated ("start_timestamp", ad_stat("start_timestamp").extract[Long]*1000)
        compact(render(new_data))
      }
    }
    
    newJson
    //////////////////////////////////////////////////////////
  }
  
  // a second benchmarking code snippet
  def timeExternalTools(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET TWO ////////////////////
    new URL("http://json-urls").#>(new File("ins.json")).!
    "cat campaigns-all.json".#|(Seq("jq", "-c", ".campaigns[] | del(._id)")).#>(new File("out.json")).!!
    
    //////////////////////////////////////////////////////////
  }
  
  // and a third benchmarking code snippet
  def timeWhile(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET THREE ////////////////////
    //TODO smth else?
    //////////////////////////////////////////////////////////
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

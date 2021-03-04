import scala.:+
import scala.collection.{mutable, _}
import scala.collection.generic._
import scala.collection.mutable.{Builder, ListBuffer}
//usuń komentarze przed oddaniem
object Histogram {
  class Hist(val number: Int, val min: Int, val max: Int) extends Traversable[(Int,Int)] { 
    self=>
    var map=mutable.HashMap[Int,Int]()
    override def toString() = {
      s"histogram min: $min max: $max bins content:" + bins.mkString(" ")
    }
     def apply(i:Int)=(i,map(i))

    override def foreach[U](f: ((Int, Int)) => U): Unit = super.foreach(f)

    def iterator: Iterator[(Int,Int)] = new AbstractIterator[(Int,Int)] {
      private var current = 0
      def hasNext = current < self.number
      def next(): (Int,Int) = {
        val elem = self(self.min+current)
        current += 1
        elem
      }
    }


    def add(values: Int*): Hist = { //funkcja dodająca * oznacza że nie znamy liczby argumentów
      if (map.isEmpty) {
        for (i <- min to max) map += (i -> 0) //tworzymy liste punktów od min do max, nadajemy wartość 0
      }
      for (value <- values if value >= min && value <= max) {
        map(value) += 1
      }
      this
    }

    def bins = map.toList
  }


  def main(args: Array[String]): Unit = {

    val studentsAge = new Hist(10, 20, 30)
    studentsAge.add(21).add(19).add(21).add(23).add(25, 26, 27, 22, 21, 19).add(26).add(31)
    println(studentsAge.bins.mkString(" "))
    println(studentsAge)

    println("count > 1 " + studentsAge.filter(_._2 > 1).mkString(" ")) // bins that have values above the threshold
    println("age > 25 " + studentsAge.filter(_._1 > 25).mkString(" ")) // bins age > 25

    println(studentsAge.maxBy(_._2)) // bin with maximum number of students
    println(studentsAge.minBy(_._2)) // --------- minimum -----------------
    println(studentsAge.toList) //
    println(studentsAge.toArray) //
    println(studentsAge.toVector) //
    println(studentsAge.exists(_._2 == 0)) // is there age bin that is unfilled
    println(studentsAge.toList.map(_._2)) // only bin values
    println(studentsAge.toList.partition(_._2 == 0)) // filled & unfilled bins
    //    All these operations filter/map etc. should come automatically from proper trait mixing i.e. you do not implement them.


}}

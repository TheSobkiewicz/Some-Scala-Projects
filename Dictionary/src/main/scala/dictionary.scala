import java.io.{File, FileOutputStream, PrintWriter}

import scala.annotation.tailrec
import scala.math.min
import scala.io.{Source, StdIn}
import scala.language.postfixOps
object dictionary {

  class Dictionary(dictionary: List[String]) {
    def Check(word: String): List[(String, Int)] = {
      dictionary.map(Laventish(_, word.toLowerCase)).sortBy(_._2).splitAt(15)._1
    }
  }

  abstract class Input() {
    def SaveInput(): String
  }

  class InputSaver(val possible: List[(String, Int)], val word: String, val DictWriter: PrintWriter) extends Input {
    @tailrec
    final def SaveInput(): String = {
      if (word != "" && possible.head._2 != 0 && !word.head.isDigit) {
        println("Save " + word + " as ")
        print("i- save as is ")
        for (a <- 0 until 15) print((a + 1) + "- " + possible(a)._1 + " ")
        val choice = StdIn.readLine()
        choice match {
          case choice if choice.matches("^[0-9]") =>
            if (choice.toInt >= 0 && choice.toInt < 15) {
              possible(choice.toInt - 1)._1
            }
            else {
              println("Something went wrong, try again :(")
              SaveInput()
            }

          case "i" =>
            DictWriter.println()
            DictWriter.print(word)
            word
          case _ =>
            println("Something went wrong, try again :(")
            SaveInput()
        }
      }
      else
      word
    }
  }
  @scala.annotation.tailrec
  def ExtractWord(pred: String = "", word: String, after: String = ""): (String, String, String) = {
    if (word.matches("(\\(|\\-).*$")) {
      ExtractWord(word.head.toString, word.tail, after)
    }
    else if (word.matches(".*(\\)|\\]|\\,|\\.)$")) {
      ExtractWord(pred, word.splitAt(word.length - 1)._1, word.last.toString)
    }
    else {
      (pred, word, after)
    }
  }
  @scala.annotation.tailrec
  def OutputWriter(DictWrite: PrintWriter, dictionary: Dictionary, Input: List[String], Output: PrintWriter) {
    for (words <- Input.head.split(" ").toList) {


      val Extracted = ExtractWord("", words, "") //pred,word,after
      val possible = dictionary.Check(Extracted._2)
      val inputSaver: InputSaver = new InputSaver(possible, Extracted._2, DictWrite)
      Output.write(" " + Extracted._1 + inputSaver.SaveInput() + Extracted._3)
    }
    Output.write("\n")
    if (Input.tail.nonEmpty) OutputWriter(DictWrite, dictionary, Input.tail, Output)
  }

  def Laventish(a: String, b: String): (String, Int) = {
    (a, ((0 to b.length).toList /: a) ((prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) => min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }) last)
  }

  def main(args: Array[String]): Unit = {
    val DictSource = Source.fromFile("dict.txt")
    val Dictionary = new Dictionary(DictSource.getLines().toList.map(_.toLowerCase))
    val DictWrite = new PrintWriter(new FileOutputStream(new File("dict.txt"), true))
    DictSource.close()
    val source = Source.fromFile(args(0))
    val Lsource = source.getLines().toList
    val Destination = new PrintWriter(args(1))
    OutputWriter(DictWrite, Dictionary, Lsource, Destination)
    source.close()
    Destination.close()
    DictWrite.close()
  }
}
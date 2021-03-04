import java.awt.Color
import java.io.PrintWriter
import scala.collection.{AbstractIterator, Iterator, Traversable, mutable}
import scala.io.Source
import scala.swing._
import javax.imageio.ImageIO
import java.io.File
import java.awt.Graphics2D
import java.awt.image.BufferedImage



object FinalHistogram {


  class Hist(val number: Int, val min: Int, val max: Int) extends Traversable[(Int, Int)] {
    self =>
    var map = mutable.HashMap[Int, Int]()

    override def toString() = {
      s"histogram min: $min max: $max bins content:" + bins.mkString(" ")
    }

    def apply(i: Int) = (i, map(i))


    def iterator: Iterator[(Int, Int)] = new AbstractIterator[(Int, Int)] {
      private var current = 0

      def hasNext = current < self.number

      def next(): (Int, Int) = {
        val elem = self(self.min + current)
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
  def ImageMaker(hist:Hist):BufferedImage={//funkcja zmieniajaca histogram w obraz

    val ysize = 600
    val xsize = 1200
    val HistYSize: Int = (ysize - 10) / hist.maxBy(_._2)._2 //obliczamy max wysokosc w oparciu o max wartośc
    val HistXSize: Int = xsize / (hist.max - hist.min + 1) //obliczamy dlugosc każdego oczka w oparciu o róznice miedze min a max
    var ActualX: Int = 0
    val image = new BufferedImage (xsize, ysize, BufferedImage.TYPE_INT_ARGB)
    val g: Graphics2D = image.createGraphics
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
      java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.clearRect(0, 0, xsize, ysize) //czyścimy ekran
    g.setColor(Color.DARK_GRAY)
    g.fillRect(0, 0, xsize, ysize) //tło
    for (x <- hist) {
      g.setColor(Color.RED);
      g.fillRect(ActualX, ysize - x._2 * HistYSize, HistXSize, x._2 * HistYSize)
      g.setColor(Color.black)
      g.drawString(x._1.toString, ActualX + HistXSize / 2 - 8, ysize - 75) //wypisujemy liczby, -75 dla estetyki
      g.setColor(Color.BLACK)
      g.drawRect(ActualX, ysize - x._2 * HistYSize, HistXSize, x._2 * HistYSize)
      ActualX += HistXSize //pokazujemy gdzie ma sie zaczac następny prostokat
    }
    g.setColor(Color.WHITE)
    g.drawString("1", 10, ysize - (HistYSize)) //wypisujemy 1 dla skali
    image//zwracamy gotowy obraz
  }
  def load(name: String): Hist = { //funkcja wczytujaca z pliku

    val source = Source.fromFile(name)
    val lhist = source.getLines().toList.map(_.toInt)
    val hist = new Hist(lhist.head, lhist(1), lhist(2))
    for (x <- 3 until lhist.length) {
      hist.add(lhist(x))
    }
    source.close()
    hist
  }

  var top: GUI = _
  class GUI(hist: Hist) extends MainFrame {
    title = "Histogram"
    val ysize = 600
    val xsize = 1200
    val HistYSize: Int = (ysize - 10) / hist.maxBy(_._2)._2 //obliczamy max wysokosc w oparciu o max wartośc
    val HistXSize: Int = xsize / (hist.max - hist.min + 1)
    var ActualX: Int = 0
    preferredSize = new Dimension(xsize, ysize+60)
    val panel = new Panel { //definujemy cokolwiek na czym można rysować
      override def paintComponent(g: Graphics2D) {
          g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
            java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
          g.clearRect(0, 0, size.width, size.height) //czyścimy ekran
          g.setColor(Color.DARK_GRAY)
          g.fillRect(0, 0, size.width, size.height) //tło
          for (x <- hist) {
            g.setColor(Color.RED);
            g.fillRect(ActualX, ysize - x._2 * HistYSize, HistXSize, x._2 * HistYSize)
            g.setColor(Color.black)
            g.drawString(x._1.toString, ActualX + HistXSize / 2 - 8, ysize-10) //wypisujemy liczby, -10 dla estetyki
            g.setColor(Color.BLACK)
            g.drawRect(ActualX, ysize - x._2 * HistYSize, HistXSize, x._2 * HistYSize)
            ActualX += HistXSize //pokazujemy gdzie ma sie zaczac następny prostokat
          }
          g.setColor(Color.WHITE)
          g.drawString("1", 10, ysize - (HistYSize)) //wypisujemy 1 dla skali
        }
    }
    contents = new BoxPanel(Orientation.Vertical) { //pionowy panel
      contents += panel
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Button(Action("Reset") {
          top.close()
          top = new GUI(load("source.txt"))
          top.visible = true
        })
        contents += new Button(Action("Save to txt file") {
          val writer = new PrintWriter("Output.txt")
          writer.write(hist.bins.mkString(" ")) //formatujemy
          writer.close()
        })
        contents += new Button(Action("Save as Image") {
          ImageIO.write(ImageMaker(hist), "png",new File( "shot.png"))//zapisujemy screena, wywołujemy funckcje zapisujaca

        })
        contents += new Button(Action("Random") {
          val NewHist = new Hist(hist.number, hist.min, hist.max) //tworzymy nowy hisogram
          val rnd = new scala.util.Random() //generator liczb losowych
          for (x <- 0 to 20) NewHist.add(hist.min + rnd.nextInt((hist.max - hist.min) + 1)) //dodajemy 20 wynikow
          top.close()
          top = new GUI(NewHist)//ładujemy nowy histogram
          top.visible = true
          top.centerOnScreen() 
        })

      }


    }
  }

  def main(args: Array[String]): Unit = {
    val hist = load("source.txt")
    println(hist)
    top = new GUI(hist)
    top.visible = true
    top.centerOnScreen()
  }

}

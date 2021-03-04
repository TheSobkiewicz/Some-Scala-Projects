import java.awt.Color

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.swing._
import math._
import scala.util.Random.nextInt

object SwingRaceDemo {

  case class NextCarPosition(m: Int)//sygnal pokazujacy pozycje auta next

  case class HONK()//sygnal do next zeby przyspieszyc

  case class AskForPosition()//zapytanie o pozycje

  case class Run()//sygnal do startu

  case class CurrentCarPosition()//sygnal dla wypisujacego loggera

  case class SetNext(car: ActorRef)//ustawienie auta na next

  var track: Track = null

  var logger: ActorRef = null

  class Logger extends Actor {//aktor odpowiedzialny na aktualizacje pozycji na torze
    def receive = {
      case m: CurrentCarPosition => {
        track.CarMoved()
      }
      case _ => println("Something went wrong in LOGGER")
    }
  }

  class Car(var degree: Int, val speed: Int, var delay: Int, var next: ActorRef = null, val color: Color) extends Actor {//klasa reprezentujaca auta na torze
    var frustiation = 0

    def x = 240 + 200 * cos(degree * Pi / 180)//funkcje wyliczajace wspolrzedne x i y

    def y = 240 + 200 * sin(degree * Pi / 180)


    def NextSet(car: ActorRef) {
      next = car
    }

    def receive = {
      case Run => {
        track.AddCar(this)
        next ! AskForPosition
      }
      case x: NextCarPosition => {
        frustiation+=speed//frustracja odpowiada za pospieszanie auta next
          for(_<-0 to speed if (degree+5)%360 != (x.m)%360 ){
            degree += 1
            frustiation -=1
          }

              if (frustiation >= 10) {//przyspieszamy auto next
                next ! HONK
                frustiation = 0
              }

          logger ! CurrentCarPosition()
          Thread.sleep(delay)

          next ! AskForPosition
        }



      case m: SetNext => {
        next = m.car
      }
      case  HONK => {
        delay = math.ceil(delay * 0.90).toInt
        //java.awt.Toolkit.getDefaultToolkit.beep
      }
      case AskForPosition => {
        sender() ! NextCarPosition(degree)
      }
      case _ => println("Something went wrong")

    }
  }

  object Car {
    def apply(degree: Int, speed: Int, delay: Int, next: ActorRef, color: Color): Car = new Car(degree, speed, delay, next, color)

  }



  class Track extends Panel {//Tor dla kulek
    var cars = List[Car]()

    def AddCar(car: Car) {
      cars = car :: cars
    }

    def CarMoved() {//aktualizowanie toru
      repaint()
    }


    override def paintComponent(g: Graphics2D) {
      g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING,
        java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
      g.clearRect(0, 0, size.width, size.height)
      g.setColor(Color.GREEN)
      g.fillRect(0, 0, size.width, size.height)//tlo
      g.setColor(Color.BLACK)
      g.fillOval(0, 0, 500, 500)//tor
      g.setColor(Color.white)
      g.drawOval(50, 50, 400, 400)//linia na srodku toru
      g.setColor(Color.gray)
      g.fillOval(100, 100, 300, 300)//srodek kola

      for (car <- cars) {
        g.setColor(car.color);
        g.fillOval(car.x.toInt, car.y.toInt, 20, 20)
      }

    }
  }


  //ToDO:minimalna odleglosc to 5

  class Race extends MainFrame {//UI

    title = "Welcome to the Race"
    preferredSize = new Dimension(520, 540)
    track = new Track
    contents = new BoxPanel(Orientation.Vertical) {
      contents += track

    }

  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("Defalut")
    logger = system.actorOf(Props[Logger])
    val top = new Race
    top.visible = true
    top.centerOnScreen()
    val vw = system.actorOf(Props(classOf[Car], 50, 2, 30, null, Color.BLUE))
    val BMW = system.actorOf(Props(classOf[Car], 30, 3, 30, vw, Color.YELLOW))
    val Jeep = system.actorOf(Props(classOf[Car], 120, 3, 40, BMW, Color.GREEN))
    val Ferrari = system.actorOf(Props(classOf[Car], 100, 3, 35, Jeep, Color.RED))
    val tesla = system.actorOf(Props(classOf[Car], 65, 3, 45, Ferrari, Color.WHITE))
    vw ! SetNext(tesla)
    Jeep ! Run
    vw ! Run
    BMW ! Run
    tesla ! Run
    Ferrari!Run


  }

}

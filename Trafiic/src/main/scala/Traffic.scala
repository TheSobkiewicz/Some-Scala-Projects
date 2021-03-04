import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case class CrashCarPosition( m: Int, name: String, frustiation: Int)

case class NextCarPosition(m:Int)
case class HONK(name:String)
case class LHonk(name:String, nextname:String)
case class CurrentCarPosition(m:Int, name:String)
case class AskForPosition()
case class Run()

object task1 {

  class Logger extends Actor {
    def receive = {
      case m: CrashCarPosition => {
        println(s"${m.name}  stucked at  ${m.m}. Frustiation raised to :${m.frustiation}")

      }
      case m:LHonk=>{
        println(s"HONK")
        java.awt.Toolkit.getDefaultToolkit.beep
        println(s"${m.name}  honked at at ${m.nextname}. ${m.nextname} speed raised")

      }
      case m: CurrentCarPosition => {
        println(s"${m.name} moved to " + m.m)
      }
      case _ => println("Something went wrong in LOGGER")
    }
  }

  var logger: ActorRef = null
  var first: ActorRef = null

  class Car(var pos: Int, var delay: Int, val next: ActorRef = null, val name: String) extends Actor {
    var frustiation=0
    def receive = {
      case Run => {
        if (next != null) next ! AskForPosition
        else self ! NextCarPosition(pos + 2)
      }
      case x: NextCarPosition => {
        if (x.m == pos + 1) {
          Thread.sleep(delay)
          frustiation+=1
          logger ! CrashCarPosition(pos, name, frustiation)
          if(frustiation==10){
            next ! HONK(name)
            frustiation=0
          }
          next ! AskForPosition
        }
        else {
          {
            Thread.sleep(delay)
            pos += 1
            logger ! CurrentCarPosition(pos, name)
            if (next != null) next ! AskForPosition
            else self ! NextCarPosition(pos + 2)
          }

        }


      }
      case m:HONK=>{
        delay=math.ceil(delay*0.9).toInt
        logger ! LHonk(m.name, name)
      }
      case AskForPosition => {
        sender() ! NextCarPosition(pos)
      }
      case _ => println("Something went wrong")

    }
  }

  object Car {
    def apply(pos: Int, delay: Int, next: ActorRef, name: String): Car = new Car(pos, delay, next, name)
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("Defult")
    val third = system.actorOf(Props(classOf[Car], 20, 450, null, "Tesla"))
    val second = system.actorOf(Props(classOf[Car], 15, 500, third, "Jeep"))
    first = system.actorOf(Props(classOf[Car], 10, 200, second, "Mustang"))
     first ! Run
    third ! Run
    second ! Run

    logger = system.actorOf(Props[Logger])


  }
}


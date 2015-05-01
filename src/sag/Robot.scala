package sag
import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer


case object Ready


/* 
 * Klasa Robota - agenta podrzednego
 */
class Robot(id: Int, info: String) extends Actor
{
	// true - wszystko ok; false - bateria rozladowana, jedz do stacji dokujacej
	var status : Boolean = true
			// Stan baterii [%]
			var batteryLevel : Int = 100
			// Polozenie robota na mapie
			var x = 0
			var y = 0


			/*
			 * Metoda oblicza dystans miedzy aktualnym polozeniem robota a celem
			 */
			def calculateDistance(produktID : Int) : Double =
		{
	val x = 1.0
			return x

		}

def act()
{

	while(true)
	{
		receive
		{
		case Hello => 
		{
			//status = false
			//println(id + ": " + info + " Status: " + status)
			sender ! Ready
			//exit()
		}

		case prods : ListBuffer[_] =>
		{
			for (p <- prods) println(p)
		}
		}
	}
}
}
package sag
import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer

/* 
 * Klasa Robota - agenta podrzednego
 */
class Robot(id: Int, info: String) extends Actor
{
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
        case Hello => println(id + ": " + info)
        exit()
      }
    }
  }
}
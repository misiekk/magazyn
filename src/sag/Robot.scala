package sag
import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer


case object Ready

/* 
 * Klasa Robota - agenta podrzednego
 */
class Robot(id_in: Int, info: String) extends Actor
{
	val id=id_in
  // true - wszystko ok; false - bateria rozladowana, jedz do stacji dokujacej
	var status : Boolean = true
  
  // Stan baterii [%]
  var batteryLevel : Int = 100
  // Polozenie robota na mapie
  var x = 0
  var y = 0
  
  //polozenie - ulamek przebytego kwadratu na mapie
  var xp=0.0
  var yp=0.0
  
  /*
   * kierunki ruchu robota
   * dx= 1 => porusza sie w prawo
   * dx=-1 => porusza sie w lewo
   * dy= 1 => porusza sie w dol
   * dy=-1 => porusza sie w gore
   * jeden z tych parametrow musi zawsze wynosic 0
   */ 
  var dx=0
  var dy=0
  
  //obecnie zajmowany kwadrat mapy; zaklada sie, ze robot moze jednoczesnie zajmowac 2 kwadraty
  var tileX1=0
  var tileX2=0
  var tileY1=0
  var tileY2=0
  
  def placeOn(tile: Tile){
     tileX1=tile.indexX
     tileY1=tile.indexY
     x=tileX1
     y=tileY1
     tile.free=false
  }
  
  def move(){
    xp+=xp*dx*Warehouse.robotsVelocity
    yp+=yp*dy*Warehouse.robotsVelocity
    //TODO: check which tile is gonna be neccessary and if it is free. change the tiles' status from busy to free or the other way around
    
  }
  /*
   * Metoda oblicza dystans miedzy aktualnym polozeniem robota a celem
   */
  def calculateDistance(produktID : Int) : Double =		{
    val x = 1.0
    return x
  }
  
  def act() {
    while(true)	{
      receive		{
        case Hello => 		{
          //status = false
          //println(id + ": " + info + " Status: " + status)
          sender ! Ready
          //exit()
        }
        case prods : ListBuffer[_] =>		{
          for (p <- prods) println(p)
        }
      }
    }
  }
}
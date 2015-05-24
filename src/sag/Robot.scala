package sag
import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer
import scala.math


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
  var x : Double = 0
  var y : Double = 0
  
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
  
  def moveUp()
  {
    dy = -1
    dx = 0
  }
  def moveDown()
  {
    dy = 1
    dx = 0
  }
  def moveLeft()
  {
    dy = 0
    dx = -1
  }
  def moveRight()
  {
    dy = 0
    dx = 1
  }
  
  /*
   * poprawka we wzorach na xp, yp
   * dodanie waunkow na zerowanie przyrostow i zmiane polozenia robota
   * */
  def move(){
    xp+=dx*Warehouse.robotsVelocity
    yp+=dy*Warehouse.robotsVelocity
    //TODO: check which tile is gonna be neccessary and if it is free. change the tiles' status from busy to free or the other way around
    x += xp
    y += yp
    
    if (xp == 1.0) 
      {
        //x += 1;
        xp = 0;
      }
    if (xp == -1.0) 
      {
        //x -= 1;
        xp = 0;
      }
    if (yp == 1.0) 
      {
        //y += 1;
        yp = 0;
      }
    if (yp == 1.0) 
      {
        //y -=1;
        yp = 0;
      }
    //println(this.id + " " + xp + " " + yp)
  }
  
  /*
   * Metoda oblicza dystans (ilosc krokow) miedzy aktualnym polozeniem robota a polka z itemem
   */
  def calculateDistance(product : Item) : Int =  {
    val s : Shelf = product.getShelf()
    if (!s.isInstanceOf[Shelf]) 
      println("Error shelf")
    // wspolrzedne polki z itemem
    var xS : Int = s.getX()
    var yS : Int = s.getY()
    
    // dystans miedzy robotem a polka liczony w liczbie krokow
    var krokiX : Int = scala.math.abs(1-2)
    var krokiY : Int = scala.math.abs(1-2)
    
    
    //println(product.getStatus())
    return krokiX + krokiY
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
        
        /*
         * item do odebrania
         * oblicz dystans do polki, na ktorej jest item
         * i odeslij masterowi odleglosc
         * TODO: robot odbiera tylko jesli nie jest zajety lub ma wysoki poziom baterii
         * */
        
        case item : Item => {
          if(status){
            //println(item.getStatus())
            // wywolac funkcje zwracajaca dystans do tego produktu i odeslac do mastera
            var dist : Double = calculateDistance(item)
            //println(item.getShelf().getX())
            //println("Robot " + id + ": " + dist)
            sender ! dist
            move()
            //println("Robot " + id + ": " + x + " " + y)
          }
        }        
      }
    }
  }
}
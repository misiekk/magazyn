package sag

import scala.actors.Actor
import scala.util.control.Breaks._
import scala.actors._
import scala.collection.mutable.ListBuffer
//import scala.collection.generic.GenericCompanion

case object Hello
case object Lol
/* 
 * Klasa Mastera - glownego agenta
 * @robots - wszystkie podlegle agenty
 */
class Master(robots: ListBuffer[Robot]) extends Actor {
  var startCheck : Int = 0
  var n = 1
  // cel, do ktorego ma dojechac item; moze niekoniecznie przechowywac to w masterze
  val goalX : Int = 10
  val goalY : Int = 10
  // lista z odleglosciami robotow od produktu (prodId, robotId, distance)
  var distanceList = new ListBuffer[(String, Int, Int)]()
  
  def act() {
    for (r <- robots)	{
      r ! Hello 
    }
    while(startCheck != robots.size)
    {
      receive
      {
        case Ready =>
          {
            startCheck += 1
            //println ("Good")
           }
        
      }
    }
    loop
    {
    receive
    {
      case infoDistance : (String, Int, Int) =>
          {
            
            println("odbieram")
            distanceList += infoDistance
            if(distanceList.size == n*robots.size)
            {
              //for(x <- distanceList)
                //println(x._1 + " " + x._2 + " " + x._3)
               
                findMinAmountOfSteps(infoDistance._1)
                n += 1
            }
            
          }
    }
    }
    //exit();
    println("Utworzonych agentow: " + startCheck)
  }
  
  def findMinAmountOfSteps(prodId : String) : (Int, Int) =
  {
      var temp = new ListBuffer[(Int, Int)]()
      for(el <- distanceList)
      {
        if(el._1 == prodId)
        {
          var x = (el._2, el._3)
          temp += x
        }
      }
      // (robotId, distance)
      var min: (Int, Int) = temp.min(Ordering.by((s : (Int, Int)) => s._2 ))
      return min
      //println("MINIMUM = Robot " + min._1 + " Dist = " + min._2)
  }
  def getProductsId()
  {
    println("Podaj ID produktow:")
    var prods = new ListBuffer[Int]()
    var x = -1
    while(x != 0)
    {
      x = readInt()
      if(x != 0) prods += x
    }
    
    for (r <- robots) r ! prods
   }
  
  // Metoda ktora uaktualnia stan w allTiles
 def findTile(t : Tile)
 {
   breakable
   {
   for (x <- Map.allTiles)
   {
     if(x.indexX == t.indexX && x.indexY == t.indexY)
       {
       x.free = false
       println("Success")
       break
       }

   }
   }
 }
  
  def placeRobots(){
     for (r <- robots)  {
       var freeTile=Map.popFreeTile()
       r.placeOn(freeTile)
       findTile(freeTile)
     }
  }
  
  /*
   * dla wszystkich wczytanych itemow 
   * znajdz ten ktory oczekuje na podjecie
   * i wyslij wszystkim robotom uchwyt do niego
   * */
  def checkProductStatusFromList()
  {
    for (i <- Warehouse.items)
    {
      //println(i.getStatus())
      if(i.getStatus() == "Awaiting pickup")
      {
        println(i.ID)
        //for (r <- robots) r ! PickProductId
        for (r <- robots) r ! i
        i.changeStatus(Status.Analysed)

       }
      
    }
  }
}
package sag

import scala.actors.Actor
import scala.util.control.Breaks._
import scala.actors._
import scala.collection.mutable.ListBuffer
//import scala.collection.generic.GenericCompanion
import scala.util._
case object Hello
case object Assign
case object ItemStatusChanged
/* 
 * Klasa Mastera - glownego agenta
 * @robots - wszystkie podlegle agenty
 */
class Master(robots: ListBuffer[Robot]) extends Actor {
  var startCheck : Int = 0

  
  var robotsNotAssigned : Int = 0
  println("SIZE = " + robots.size)
  // wspolrzedne do ktorych robot z itemem ma dojechac
  val warehouseGoalX = 13
  val warehouseGoalY = 8
  // lista z odleglosciami robotow od produktu (prodId, robotId, distance)
  var distanceList = new ListBuffer[(String, Int, Int)]()
  
  //
  var itemsToPick = new ListBuffer[(Item)]()
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
          }
      }
    }
    robotsNotAssigned = robots.size
    println("Not Assigned: " + robotsNotAssigned)
    loop
    {
    receive
    {
      case infoDistance : (String, Int, Int) =>
          {
            //println("odbieram")
            distanceList += infoDistance
            if(distanceList.size == robotsNotAssigned)
            {             
              println("DistList size = " + distanceList.size + ", Not Assigned " + robotsNotAssigned)
                var result = findMinAmountOfSteps(infoDistance._1)
                //n += 1
                
                breakable
                {
                for(r<-robots)
                {
                  // dla robota z najmniejsza iloscia krokow
                  // przypisz produkt i rozpocznij misje
                  if(r.id == result._1)
                  {
                    r ! (infoDistance._1)
                    break
                  }
                }
                }
                distanceList.clear()
                //println(distanceList.size)
                
                  
            }
          }
      case Ready => 
        {
          if(robotsNotAssigned < robots.size)
            robotsNotAssigned += 1
          else
          println("Problem z licznikiem assigned Ready")
          println("ready again " + robotsNotAssigned)
        }
      case Busy =>
        {
          if(robotsNotAssigned > 0)
            robotsNotAssigned -= 1
          else
            println("Problem z licznikiem assigned Busy")
          println("busy " + robotsNotAssigned)
        }
      case Refuse =>
        {
          // nic nie rob
        }
      case ItemStatusChanged =>
        {
          // wyslij zadanie odswiezenia listy statusow itemow
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
      println("MINIMUM = Robot " + min._1 + " Dist = " + min._2)
      return min
      
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
  def checkProductStatusFromList(item : Item)
  {
    var licznik : Int = 0

    //for (r <- robots) r ! PickProductId
    for (r <- robots) 
      {
      // a jesli zaden nie jest wolny to zaden nie dostanie info
      // jakis mechanizm powtarzajacy
      if(r.status)
      {
        r ! item
        item.changeStatus(Status.AwaitingPickup)
        licznik += 1
      }
      }
    // dodaj do listy awaiting pickup
    if(licznik == 0)
    {
      itemsToPick += item
    }

  }
  
  def sendItemToRobots()
  {
    
  }
}
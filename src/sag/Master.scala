package sag

import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer
//import scala.collection.generic.GenericCompanion

case object Hello

/* 
 * Klasa Mastera - glownego agenta
 * @robots - wszystkie podlegle agenty
 */
class Master(robots: ListBuffer[Robot]) extends Actor {
  var startCheck : Int = 0
  
  // cel, do ktorego ma dojechac item; moze niekoniecznie przechowywac to w masterze
  val goalX : Int = 10
  val goalY : Int = 10
  // lista(?) z odleglosciami robotow od produktu
  
  
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
    //exit();
    println("Utworzonych agentow: " + startCheck)
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
  
  def placeRobots(){
     for (r <- robots)  {
       var freeTile=Map.popFreeTile()
       r.placeOn(freeTile)
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
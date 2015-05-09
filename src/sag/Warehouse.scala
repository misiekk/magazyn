package sag

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks._

object Status extends Enumeration {
  val InStore, AwaitingPickup, EnRoute, Delivered = Value
}

//a class representing an item placed on a shelf. After an item is delivered to the warehouse manager, it stops being a shelf item
class Item(ID_in: String){
  val ID:String=ID_in
  var shelf: Shelf=null
  var status:Status.Value=Status.InStore
  
  def putOnShelf(shelfIn: Shelf): Boolean={
    if(shelfIn.receiveItem(this)){
      shelf=shelfIn
      return true
    }
    return false
  }
  
  def changeStatus(status_in: Status.Value){
    status=status_in
  }
  
  def getStatus(): String={
    if(status==Status.InStore)
      return "In Store"
    else if(status==Status.AwaitingPickup)
      return "Awaiting pickup"
    else if(status==Status.EnRoute)
      return "En route"
    else if(status==Status.Delivered)
      return "Delivered"
    else
      return "Error"
  }
}

class Shelf(val xc: Int, val yc: Int) {
   var x: Int = xc
   var y: Int = yc
   val maxItemsProShelf: Int =10
   var items=new ListBuffer[Item]()
    
   def receiveItem(item: Item): Boolean={
      if(items.length>=maxItemsProShelf){
        items= items:+item
        return true
      }
      return false
   }
}

object Warehouse {
  var agentsNum:Int=0
  var agentList = new ListBuffer[Robot]()
  var master:Master=null
  var shelves=List[Shelf]()
  var items=new ListBuffer[Item]()
  
  //initialize shelves
  private var indexX=0
  private var indexY=0
  private var lineTemp=""
  for(line <- Source.fromFile("layoutB.txt").getLines()){
     indexX=0 
     for (nr <- line){
       if(nr=='1')
         shelves=shelves :+ new Shelf(indexX, indexY)          
         indexX+=1          
     }      
     indexY+=1
     lineTemp=line
  }
   val x_len_param=lineTemp.length()
   val y_len_param=indexY
    
   //initialize items
  for(line <- Source.fromFile("items.txt").getLines())
    items= items :+new Item(line)  

  //put items on shelves
  for(item <- items){
   //println("_" + item.ID+ "_")
    breakable {for (shelf <- shelves){
     if (item.putOnShelf(shelf)) 
       break
    } }
  }
   
   def getItem(ID: String): Item={
     for (item <- items){
       if(ID==item.ID)
         return item
     }
     return null
   }
   
   def initAgents(){
     for(x <- 1 to agentsNum)
       agentList += new Robot(x, "Robot " + x)         
   }
   
   def initMaster(){
     master=new Master(agentList)
     master.start()
   }
  
}










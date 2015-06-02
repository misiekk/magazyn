package sag
import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer
import scala.math
import scala.util._
import scala.util.control.Breaks._
case object Ready

/* 
 * Klasa Robota - agenta podrzednego
 */
class Robot(id_in: Int, master: Master) extends Actor {
  val id = id_in
  // true - wszystko ok; false - bateria rozladowana, jedz do stacji dokujacej
  var status: Boolean = true

  var goingUp, goingDown, goingLeft, goingRight : Boolean = false
  // ile razy wywolalo move, tymczasowe lipne rozwiazanie
  var counter : Int = 0
  // cel, do ktorego ma dojechac robot (uaktualniane, gdy dostanie zadanie od mastera)
  var xGoal: Int = -1
  var yGoal: Int = -1
  // Stan baterii [%]
  var batteryLevel: Int = 100
  // Polozenie robota na mapie
  var x: Double = 0.0
  var y: Double = 0.0
  
  //polozenie - ulamek przebytego kwadratu na mapie
  var xp = 0.0
  var yp = 0.0
  
  var xMove = 0.0
  var yMove = 0.0
  var firstTime = true
  /*
   * kierunki ruchu robota
   * dx= 1 => porusza sie w prawo
   * dx=-1 => porusza sie w lewo
   * dy= 1 => porusza sie w dol
   * dy=-1 => porusza sie w gore
   * jeden z tych parametrow musi zawsze wynosic 0
   */
  var dx = 0
  var dy = 0

  //obecnie zajmowany kwadrat mapy; zaklada sie, ze robot moze jednoczesnie zajmowac 2 kwadraty
  var tileX1 : Int = -1
  var tileY1 : Int = -1
  
  var tileX2 : Int = -1  
  var tileY2 : Int = -1

  def placeOn(tile: Tile) {
    tileX1 = tile.indexX
    tileY1 = tile.indexY
    //tileX2 = tile.indexX
    //tileY2 = tile.indexY
    x = tileX1
    y = tileY1
    tile.free = false
  }

  def moveUp() {
    dy = -1
    dx = 0
    goingUp = true
    goingDown = false
    goingLeft = false
    goingRight = false
  }
  def moveDown() {
    dy = 1
    dx = 0
    goingUp = false
    goingDown = true
    goingLeft = false
    goingRight = false
  }
  def moveLeft() {
    dy = 0
    dx = -1
    goingUp = false
    goingDown = false
    goingLeft = true
    goingRight = false
  }
  def moveRight() {
    dy = 0
    dx = 1
    goingUp = false
    goingDown = false
    goingLeft = false
    goingRight = true
  }
  def resetDirections()
  {
    dy = 0
    dx = 0
    goingUp = false
    goingDown = false
    goingLeft = false
    goingRight = false
  }

  // do debugowania
  def printOccupiedTiles()
  {
    for(a <- Map.allTiles)
    {
      if(!a.free)
        println(a.indexX + " " + a.indexY)
    }
    println("---")
 }
 
  /*
   * poprawka we wzorach na xp, yp
   * */
  def move() {
    xMove = dx * Warehouse.robotsVelocity
    yMove = dy * Warehouse.robotsVelocity
    xp += dx * Warehouse.robotsVelocity
    yp += dy * Warehouse.robotsVelocity

    //TODO: check which tile is gonna be neccessary and if it is free. change the tiles' status from busy to free or the other way around
    x += xp
    y += yp
    // ucinanie dupnych czesci ulamkowych
    x = BigDecimal(x).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    y = BigDecimal(y).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    
    xMove = BigDecimal(xMove).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    yMove = BigDecimal(yMove).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    xp = 0
    yp = 0
    
    //println("(R" + id + ") x, y = " + x + ", " + y)
    //xp = 0.0
    //yp = 0.0
    //printOccupiedTiles()
    counter += 1
    // TODO
    // sprawdz w ktorej komorce jest robot i ustaw odpowiednia flage zajetosci
    // podczas ruchu robota zajmuje on zawsze 2 komorki
    
    //println("x, y = " + x + ", " + y)
    /*
    if (xp == 1.0) {
      //x += 1;
      xp = 0;
    }
    if (xp == -1.0) {
      //x -= 1;
      xp = 0;
    }
    if (yp == 1.0) {
      //y += 1;
      yp = 0;
    }
    if (yp == 1.0) {
      //y -=1;
      yp = 0;
    }*/
    
    //println(this.id + " " + xp + " " + yp)
  }

  /*
   * TODO
   * Metoda oblicza dystans (ilosc krokow) miedzy aktualnym polozeniem robota a polka z itemem
   */
  def calculateDistance(product: Item): Int = {
    val s: Shelf = product.getShelf()
    if (!s.isInstanceOf[Shelf])
      println("Error shelf")
    // wspolrzedne polki z itemem
    var xS: Int = s.getX()
    var yS: Int = s.getY()

    // TODO 
    // dystans miedzy robotem a polka liczony w liczbie krokow
    // jakis problem z wyborem typu mialem, brakowalo koncepcji 
    var krokiX: Int = scala.math.abs(tileX1 - xS)
    var krokiY: Int = scala.math.abs(tileY1 - yS)
    var suma = krokiX + krokiY
    //println("Robot " + id + " (kroki): " + suma)
    
    return suma
  }

  /*
   * Metoda wywolywana jesli robot dostanie zadanie; ustawia wspolrzedne celu
   * */
  def setGoalCords(item: Item) {
    if(item.isInstanceOf[Item])
    {
      xGoal = item.getShelf().getX()
      yGoal = item.getShelf().getY()
      println("CEL: (" + xGoal + ", " + yGoal+ ")")
    }
    else
      println("ERROR WHILE SETTING GOAL CORDS!")
 }
  
  // Metoda ustawia flage zajetosci dla komorki
  def setTileOccupied(tile: Tile) : Boolean =
  {
    //println("jestem")
    if(tile.free)
    {
      tile.free = false
      return true
    }
    
    return false
  }
  
  /*
   * Metoda sprawdza czy robot dojechal do tile'a X2, Y2
   * Jesli tak to osiagniety tile staje sie X1, Y1
   * i ustawiamy flage free do starego tile'a
   */
  
  def checkIfReachedSecondTile() : Boolean = 
  {
    // jesli dojechalismy do drugiego tile'a
    if(x == tileX2 && y == tileY2)
    {
      //println("tile1 " + tileX1 + " " + tileY1 + "; tile2 " + tileX2 + " " + tileY2)
      // to znajdz pierwszego tile'a 
      //breakable
      //{
      for (t <- Map.allTiles)
      {
        /*if(tileX2 == t.indexX && tileY2 == t.indexY)
          t.free =false*/
        if(tileX1 == t.indexX && tileY1 == t.indexY)
        {
          this.synchronized
          {
          // zwolnij go
          t.free = true
          // i odswiez pozycje robota 
          tileX1 = tileX2
          tileY1 = tileY2
          t.robotId = 0
          }
          //println("Robot " + id + ": " + "Dojechalem do " + tileX2 + " " + tileY2)
          //println("Robot " + id + ": " +  t.indexX + " " + t.indexY + " zwolniona")
          return true
        }
      }
      //}
    }
    return false
  }

  /*
   * Metoda sprawdza czy prawy kwadrat od robota jest wolny
   */
  def checkRightTile() : Boolean =
  {
    // sprawdzic czy w Map.allTiles jest kafelek x+1 i czy jest wolny
    // TODO: uwaga na wartosci skrajne mapy
    //println("prawa")
    for (t <- Map.allTiles)
      {
      //println("22222")
      // jesli komorka wolna
        if((tileX1 + 1 == t.indexX && tileY1 == t.indexY && t.free))
        {
          //this.synchronized
          //{
          //println("aaa")
          // jesli nie zajeta (podwojne sprawdzenie) to zmien status na zajeta
          if(setTileOccupied(t))
          {
            //println("bbb")
            // jesli zajmiemy komorke to przypisujemy ja jako druga dla robota
            // trzeba zwolnic stare x1, y1 jesli dotrzemy do x2,y2 i do x1, y1 przypisac obecna wartosc z x2,y2
            tileX2 = t.indexX
            tileY2 = t.indexY
            t.robotId = id
            
            //println("33333")
            return true
          }
          else
            return false
          //}
        }
        // jesli juz zajelismy w pierwszym ifie to tam zmierzamy (mechanizm anty-samoblokujacy)
        if(tileX2 == t.indexX && tileY2 == t.indexY && !t.free && t.robotId == id && goingRight)
        {
          //updateTile1()
          //println(tileX2 + " " + tileY2)
          return true
        }
      }
    return false
  }
  /*
   * Metoda sprawdza czy lewy kwadrat od robota jest wolny
   */
  def checkLeftTile() : Boolean =
  {
    
    for (t <- Map.allTiles)
      {
        if((tileX1 - 1 == t.indexX && tileY1 == t.indexY && t.free)) 
        {
          println("check left tile")
          if(setTileOccupied(t))
          {
            tileX2 = t.indexX
            tileY2 = t.indexY
            t.robotId = id
            return true
          }
          else
            return false
        }
        if(tileX2 == t.indexX && tileY2 == t.indexY && !t.free && t.robotId == id && goingLeft)
        {
          //updateTile1()
          return true
        }
      }
    return false
  }
  /*
   * Metoda sprawdza czy gorny kwadrat od robota jest wolny
   */
  def checkTopTile() : Boolean =
  {
    for (t <- Map.allTiles)
      {
        if((tileX1  == t.indexX && tileY1 - 1 == t.indexY && t.free))
        {
          if(setTileOccupied(t))
          {
            tileX2 = t.indexX
            tileY2 = t.indexY
            t.robotId = id
            return true
          }
          else
            return false
        }

        if(tileX2 == t.indexX && tileY2 == t.indexY && !t.free && t.robotId == id && goingUp)
        {
          //updateTile1()

          return true
        }
      }
    return false
  }
  /*
   * Metoda sprawdza czy dolny kwadrat od robota jest wolny
   */
  def checkBottomTile() : Boolean =
  {
    for (t <- Map.allTiles)
      {
        if((tileX1  == t.indexX && tileY1 + 1 == t.indexY && t.free))
        {
          if(setTileOccupied(t))
          {
            tileX2 = t.indexX
            tileY2 = t.indexY
            t.robotId = id
            return true
          }
          else
            return false
        }
        if(tileX2 == t.indexX && tileY2 == t.indexY && !t.free && t.robotId == id && goingDown)
        {
          //updateTile1()
          return true
        }
      }
    return false
  }
  
  def setXpYpToZero()
  {
    xMove = 0
    yMove = 0
    counter = 0
  }
  
  def updateTile1()
  {
    if (x == tileX2 && y == tileY2)
    {
      tileX1 = tileX2
      tileY1 = tileY2
    }
  }
  
  /*
   * Metoda sprawdza, w jakim kierunku powinien pojechac robot
   * i ustawia odpowiednie wartosci dx, dy
   * musi tez sprawdzac, czy komorka do ktorej wstepnie jedziemy jest wolna;
   * jesli nie to szuka kolejnego kierunku;
   * TODO: jesli nie ma takiego to wysyla w swiat info "posun dupe ty, ktory jestes w komorce (x, y)"
   * */
  def setDirection(): Boolean =
  {
  //println("hop")
    if (isGoalReached())
    {
      resetDirections()
      return false

    }
    // zmiana kierunku dopiero po osiagnieciu pelnej komorki tile2
    /*if(!checkIfReachedSecondTile())
      return true;
    else
    {*/
      if (xGoal != x)// && (xMove == 1.0 || firstTime))
      {
        //updateTile1()
        //println(id + " po x")
        setXpYpToZero()
        
        if (xGoal > x) {
          //if(id == 1) println("ahoj")
          
          firstTime = false
          if(checkIfReachedSecondTile())
            tileX2 += 1
          if(checkRightTile())
          {
          moveRight()
            
          //println("Tx1 = " + tileX1 + " Tx2 = " + tileX2)
          return true
          }
        }
        if (xGoal < x) {
          //println("w lewo")
          
          firstTime = false
          if(checkIfReachedSecondTile())
            tileX2 -= 1
          if(checkLeftTile()) 
          {
            moveLeft()
            return true
          }
        }
      }
      if (yGoal != y)// && (yMove == 1.0 || firstTime))
      {
        //updateTile1()
        setXpYpToZero()
        if (yGoal > y) {
          
          firstTime = false
          if(checkIfReachedSecondTile())
            tileY2 += 1
            
          if(checkBottomTile())
          {
          moveDown()
          return true
          }
        }
        if (yGoal < y) {
          
          firstTime = false
          if(checkIfReachedSecondTile())
            tileY2 -= 1
          
          if(checkTopTile())
          {
          moveUp()
          return true
          }
        }
      }
    //}
    //if(dirSet)
     // return true

    return false
  }

  def isGoalReached() : Boolean =
  {
    if(x == xGoal && y == yGoal)
    {
      return true
    }
    
    return false
  }
  
  
  def isNextHopReached : Boolean = 
  {
    if( xp == 0.0 || yp == 0.0)
    {
      return true
    }
    return false
  }
  
def findItemFromString(name : String) : Item = 
{
  for(i <- Warehouse.items)
  {
    if (i.ID == name)
      return i
  }
  return null
}

def setGoalToMan()
{
   xGoal = master.warehouseGoalX
   yGoal = master.warehouseGoalY
}
  
def go()
{
  while(!isGoalReached())
    {
        if(setDirection() )
        {
          //println("Driving home for christmas...")
          move()
        }
      Thread.sleep(40)
    }
    println("R" + id + " za whilem")
    
    // zwolnij T1
    breakable
    {
    for (t <- Map.allTiles)
    {
      if(tileX1 == t.indexX && tileY1 == t.indexY)
      {
        
        //println("Robot " + id + ": " +  "Ostatnio zwolniona: " + t.indexX + " " + t.indexY)
        if(tileX2 != -1 || tileY2 != -1)
        {
        tileX1 = tileX2
        tileY1 = tileY2
        t.free = true
        t.robotId = 0
        }
        tileX2 = -1
        tileY2 = -1
        
        
        //println("Robot " + id + ": " +  "ostatnie T1 = " + tileX1 + " " + tileY1)
        break
      }
    }
    }  
}
  def act() {
    while (true) {
      receive {
        case Hello => {
          //status = false
          //println(id + ": " + info + " Status: " + status)
          sender ! Ready
          //exit()
        }
        case prods: ListBuffer[_] => {
          for (p <- prods) println(p)
        }

        /*
         * item do odebrania
         * oblicz dystans do polki, na ktorej jest item
         * i odeslij masterowi odleglosc
         * TODO: robot odbiera tylko jesli nie jest zajety lub ma wysoki poziom baterii
         * */
        case item: Item => {
          if (status) {
            // liczymy odleglosc do item'a
            var dist: Int = calculateDistance(item)
            val infoDistance = (item.ID, id, dist)
            //println(infoDistance._1 + " " + infoDistance._2)
            master ! infoDistance
     
            //
          }
        }
        
        case startMission : String =>
          {
            println("Assign robot: " + id + " for product: " + startMission)
            setGoalCords(findItemFromString(startMission))
            
            //println("Robot " + id + ": " + x + " " + y + " T1: " + tileX1 + " " + tileY1 + " T2: " + tileX2 + " " + tileY2)
            go()
            
            
            // po dotarciu do celu poczekaj chwile, zmien cel na magazyniera i jedz do niego
            Thread.sleep(1000)
            setGoalToMan()
            go()

          }
      }
    }
  }
}
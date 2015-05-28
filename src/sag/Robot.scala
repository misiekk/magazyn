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
class Robot(id_in: Int, info: String) extends Actor {
  val id = id_in
  // true - wszystko ok; false - bateria rozladowana, jedz do stacji dokujacej
  var status: Boolean = true

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
  }
  def moveDown() {
    dy = 1
    dx = 0
  }
  def moveLeft() {
    dy = 0
    dx = -1
  }
  def moveRight() {
    dy = 0
    dx = 1
  }
  def resetDirections()
  {
    dy = 0
    dx = 0
  }

  /*
   * poprawka we wzorach na xp, yp
   * */
  def move() {
    xp += dx * Warehouse.robotsVelocity
    yp += dy * Warehouse.robotsVelocity

    //TODO: check which tile is gonna be neccessary and if it is free. change the tiles' status from busy to free or the other way around
    x += xp
    y += yp
    // ucinanie dupnych czesci ulamkowych
    x = BigDecimal(x).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    y = BigDecimal(y).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    
    //println("x, y = " + x + ", " + y)
    xp = 0.0
    yp = 0.0
    
    
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
    var krokiX: Int = scala.math.abs(1 - 2)
    var krokiY: Int = scala.math.abs(1 - 2)

    //println(product.getStatus())
    return krokiX + krokiY
  }

  /*
   * Metoda wywolywana jesli robot dostanie zadanie; ustawia wspolrzedne celu
   * */
  def setGoalCords(item: Item) {
    xGoal = item.getShelf().getX()
    yGoal = item.getShelf().getY()
    println("CEL: (" + xGoal + ", " + yGoal+ ")")
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
        if(tileX1 == t.indexX && tileY1 == t.indexY)
        {
          // zwolnij go
          t.free = true
          // i odswiez pozycje robota 
          tileX1 = tileX2
          tileY1 = tileY2
          
          //break

          println("Robot " + id + ": " + "Dojechalem do " + tileX2 + " " + tileY2)
          println("Robot " + id + ": " +  t.indexX + " " + t.indexY + " zwolniona")
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
          //println("aaa")
          // jesli nie zajeta (podwojne sprawdzenie) to zmien status na zajeta
          if(setTileOccupied(t))
          {
            //println("bbb")
            // jesli zajmiemy komorke to przypisujemy ja jako druga dla robota
            // trzeba zwolnic stare x1, y1 jesli dotrzemy do x2,y2 i do x1, y1 przypisac obecna wartosc z x2,y2
            tileX2 = t.indexX
            tileY2 = t.indexY
            
            //println("33333")
            return true
          }
          else
            return false
        }
        // jesli juz zajelismy w pierwszym ifie to tam zmierzamy (mechanizm anty-samoblokujacy)
        if(tileX2 == t.indexX && tileY2 == t.indexY)
        {
          //println(tileX2 + " " + tileY2)
          return true
        }
      }
    return false
    //return true
  }
  /*
   * Metoda sprawdza czy lewy kwadrat od robota jest wolny
   */
  def checkLeftTile() : Boolean =
  {
    //println("lewa")
    
    return true
  }
  /*
   * Metoda sprawdza czy gorny kwadrat od robota jest wolny
   */
  def checkTopTile() : Boolean =
  {
    //println("gora")
    return true
  }
  /*
   * Metoda sprawdza czy dolny kwadrat od robota jest wolny
   */
  def checkBottomTile() : Boolean =
  {
    //println("dol")
    return true
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
    else if (xGoal != x)
    {
      if (xGoal > x && checkRightTile()) {
        //if(id == 1) println("ahoj")
        moveRight()
        if(checkIfReachedSecondTile())
          tileX2 += 1
          
        //println("Tx1 = " + tileX1 + " Tx2 = " + tileX2)
        return true
      }
      else if (xGoal < x && checkLeftTile()) {
        moveLeft()
        return true
      }
    }
    else if (yGoal != y)
    {
      if (yGoal > y && checkBottomTile()) {
        moveDown()
        return true
      }
      else if (yGoal < y && checkTopTile()) {
        moveUp()
        return true
      }
    }
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
            //println(item.getStatus())
            // wywolac funkcje zwracajaca dystans do tego produktu i odeslac do mastera
            var dist: Double = calculateDistance(item)
            //println(item.getShelf().getX())
            //println("Robot " + id + ": " + dist)

            sender ! dist

            // funkcja pobierajaca wspolrzedne celu
            setGoalCords(item)
            //while (x != xGoal || y != yGoal) 
            
            println("Robot " + id + ": " + x + " " + y)

            while(!isGoalReached())
            {
              if(setDirection())
              {
                //println("Driving home for christmas...")
                move()
              }

              Thread.sleep(40)
            }
            println("za whilem")
            
            // zwolnij T1
            breakable
            {
            for (t <- Map.allTiles)
            {
              if(tileX1 == t.indexX && tileY1 == t.indexY)
              {
                t.free = true
                println("Robot " + id + ": " +  "Ostatnio zwolniona: " + t.indexX + " " + t.indexY)
                tileX1 = tileX2
                tileY1 = tileY2
                println("Robot " + id + ": " +  "ostatnie T1 = " + tileX1 + " " + tileY1)
                break
              }
            }
            }
          }
        }
      }
    }
  }
}
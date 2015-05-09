package sag
import scala.swing.Panel
import java.awt.{ Graphics2D, Color }
import scala.swing._

class Canvas (shelves_in: List[Shelf]) extends Panel {
 // val x=3
//  val w=9+4*x //width
  val h=20
  var a=(size.getHeight()*0.05).toInt
  
  
  var centerColor = Color.yellow

  var shelves=shelves_in
  //var robots
  override def paintComponent(g: Graphics2D) { 
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.DARK_GRAY)  
    g.fillRect(0, 0, size.width, size.height)
    g.setColor(Color.BLACK) 
  //  g.drawRect(0, 0, size.width, size.height)
 ////   g.setColor(Color.blue)
 //   g.fillRect(100, 0, a*2, a*2)
 //   g.fillOval(0, 0, 100, 100)
 //   g.setColor(Color.red)
 //   g.fillOval(20, 20, 60, 60)
 //   g.setColor(centerColor)
 //   g.fillOval(40, 40, 20, 20)
    
    // Draw things that change on top of background
   /*( for (robot <- robots) {
      g.setColor(dart.color)
      g.fillOval(dart.x, dart.y, 10, 10)
    }*/
    
    for(shelf <- shelves){     
      g.setColor(Color.ORANGE)
      g.fillRect(shelf.x*a, shelf.y*a, a, a)
      g.setColor(Color.BLACK)
      g.drawRect(shelf.x*a, shelf.y*a, a, a)
    }
  }
    
  def setTheSize(x: Int, y: Int){
   size.setSize(new Dimension(x*a,y*a))  
   //preferredSize=new Dimension(x*a,y*a)
   repaint()
  }
}
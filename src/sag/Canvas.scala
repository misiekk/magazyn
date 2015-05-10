package sag
import scala.swing.Panel
import java.awt.{ Graphics2D, Color }
import scala.swing._

class Canvas (shelves_in: List[Shelf]) extends Panel {
 // val x=3
//  val w=9+4*x //width
  val h=20
  
  //lenght/width of a tile
  var a=(size.getHeight()*0.05).toInt
  
  //radius for drawing robots
  //var a_r=(size.getHeight()*0.05*0.75).toInt
   var a_r=a
  
  
  var centerColor = Color.yellow

  var shelves=shelves_in
 
  def resizeParams(){
   var temp=0.0
   if(size.getWidth()>size.getHeight())
     temp=size.getHeight()
   else
     temp=size.getWidth()
   a= (temp*0.05).toInt
   //a_r=(temp*0.05*0.5).toInt
   a_r=a
   repaint()
  }
  
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
    
    //x and y are the coordinates of the upper left corner of a rectangle on which the ellipse is being drawn
    if(Warehouse.agentList!=null) {
      for (robot <- Warehouse.agentList){
        var x=((robot.x+robot.xp)*a).toInt
        var y=((robot.y+robot.yp)*a).toInt
        g.setColor(Color.RED)
        g.fillOval(x, y, a_r, a_r)
     }
   }
  }
    
  def setTheSize(x: Int, y: Int){
   size.setSize(new Dimension(x*a,y*a))  
   //preferredSize=new Dimension(x*a,y*a)
   repaint()
  }
}
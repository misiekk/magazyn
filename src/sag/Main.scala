package sag

import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer

//graphics
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import scala.swing.Panel
import java.awt.{ Graphics2D, Color }

import scala.io.Source
import ListView._

import swing._
import swing.event._
import GridBagPanel._
import java.awt.Insets

import scala.collection
import java.io._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

case class Dart(val x: Int, val y: Int, val color: java.awt.Color)

class Shelf(val xc: Int, val yc: Int) {
   var x: Int = xc
   var y: Int = yc
   def move(dx: Int, dy: Int) {
      x = x + dx
      y = y + dy
      println ("Point x location : " + x);
      println ("Point y location : " + y);
   }
}

//------------------------canvas--------------------------
class Canvas extends Panel {
 // val x=3
//  val w=9+4*x //width
  val h=20
  val a=20
  
  var centerColor = Color.yellow
  
  var darts = List[Dart]()
  var shelves=List[Shelf]()
  override def paintComponent(g: Graphics2D) {
    
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.DARK_GRAY)
 //   g.drawRect(0, 0, size.width, size.height)
    g.fillRect(0, 0, size.width, size.height)
 //   g.setColor(Color.blue)
 //   g.fillRect(100, 0, a*2, a*2)
 //   g.fillOval(0, 0, 100, 100)
 //   g.setColor(Color.red)
 //   g.fillOval(20, 20, 60, 60)
 //   g.setColor(centerColor)
 //   g.fillOval(40, 40, 20, 20)
    
    // Draw things that change on top of background
    for (dart <- darts) {
      g.setColor(dart.color)
      g.fillOval(dart.x, dart.y, 10, 10)
    }
    
    for(shelf <- shelves){
      
      g.setColor(Color.ORANGE)
      g.fillRect(shelf.x*a, shelf.y*a, a, a)
      g.setColor(Color.BLACK)
      g.drawRect(shelf.x*a, shelf.y*a, a, a)
    }
  }

  /** Add a "dart" to list of things to display */
  def throwDart(dart: Dart) {
    darts = darts :+ dart
    // Tell Scala that the display should be repainted
    repaint()
  }
  
  def initShelf(shelf: Shelf){
    shelves=shelves :+ shelf
  }
  
  def setTheSize(x: Int, y: Int){
   size.setSize(new Dimension(x*a,y*a))
   repaint()
   // preferredSize=new Dimension(x*a,y*a)
  }
}

//-----------------end canvas--------------------


object main extends SimpleSwingApplication {
  
  //logic
  var exit = false
  var inputAgents=0
  val AgentList = new ListBuffer[Robot]()

  
  
  def top = new MainFrame { // top is a required method
    title = "Magazynn"
    this.peer.setLocationRelativeTo(null)
    this.peer.setPreferredSize(new Dimension(800,600))
    
    // declare Components here
    val canvas=new Canvas
    
    //initialize shelves
    var indexX=0
    var indexY=0
    var line: String=""
    for(line <- Source.fromFile("layoutA.txt").getLines()){
      indexX=0 
      for (nr <- line){
        if(nr=='1')
          canvas.initShelf(new Shelf(indexX, indexY))          
          //println(indexX)
          indexX+=1          
      }
      
      indexY+=1       
    }
      
    canvas.setTheSize(line.length(), indexY)
  
    val ui=new GridBagPanel{
      val c=new Constraints
       
      val lblItem= new Label {text="Enter item number:"}
      val txtItemNum=new TextField
      val btnGetItem=new Button { text="Get item"}
      val getItemPanel= new GridPanel(1,3){
        contents += lblItem
        contents += txtItemNum
        contents += btnGetItem
        this.hGap=10
        background=Color.YELLOW
        xLayoutAlignment=10
      }  
      c.gridx = 0
      c.gridy = 0
      c.gridwidth=3
      c.gridheight=1
      c.weightx = 0
      c.weighty=0.05    
      layout(getItemPanel) = c
              
      val lblWaiting= new Label("Waiting for")
      val lblItems=new Label{text="Items:"}
     // val lblReady=new Label{text="Ready:"}
      val txtItems=new TextArea
      val itemsPanel=new BoxPanel(Orientation.Vertical){
        contents+=lblItems
       // contents+=lblReady
        contents+=txtItems
        background=Color.PINK 
      }   
      c.fill = Fill.Both
      c.gridx = 0
      c.gridy = 1
      c.gridwidth=1
      c.gridheight=2
      c.weightx = 0.125
      c.weighty=0.95   
      layout(itemsPanel)=c
      
      val btnTemp=new Button("canvas substitiute")     
      c.gridx = 1
      c.gridy = 1
      c.gridwidth=1
      c.gridheight=2
      c.weightx = 0.75
      c.weighty=0.95 
      layout(btnTemp)=c
      
      val lblAgentsNum=new Label("Enter the number of robots")
      val txtAgentsNum=new TextField
      val btnGetAgentsNum=new Button("OK")
      val txtStats=new TextArea {background = Color.pink}
      val getAgentsNumPanel= new GridPanel(3,1){
        contents += lblAgentsNum
        contents += txtAgentsNum
        contents += btnGetAgentsNum
        border = Swing.LineBorder(Color.BLACK)
        background=Color.ORANGE
        vGap=0
      }
      c.gridx = 2
      c.gridy = 1
      c.gridwidth=1
      c.gridheight=1
      c.weightx = 0.125
      c.weighty=0.1
      layout(getAgentsNumPanel)=c
      
      c.gridx = 2
      c.gridy = 2
      c.gridwidth=1
      c.gridheight=1
      c.weightx = 0.125
      c.weighty=0.85
      layout(txtStats)=c
         
      listenTo(btnGetItem)
      listenTo(btnGetAgentsNum)
      reactions+={
        //agentsNum defined
        case ButtonClicked(component) if component == btnGetAgentsNum =>      
          if (txtAgentsNum.text!=""){
            btnGetAgentsNum.enabled_=(false)
            inputAgents=txtAgentsNum.text.toInt       
             txtStats.append("Number of robots is: " + txtAgentsNum.text)
             for( x <- 1 to inputAgents)  
              AgentList += new Robot(x, "Robot " + x)
             txtAgentsNum.text=""       
          }
        //item requested:
        case ButtonClicked(component) if component == btnGetItem =>
          txtItems.append(txtItemNum.text +"\n")
          txtItemNum.text=""
      }
    }
    contents=ui
      
   
   
      
      val myListView = new ListView[String]() {
      var myListBuffer = scala.collection.mutable.ListBuffer("Paris", "New York", "Tokyo", "Berlin", "Copenhagen")
      listData = myListBuffer
      listenTo(mouse.clicks)
      listenTo(this.selection)
     // listenTo(myListView)
     
      selection.reactions += {
      case event.ListSelectionChanged(source, range, _) =>
        if(!range.isEmpty) {
         // println("selection " + range)
          
        }
    //  case SelectionChanged => this.selection.items.headOption.foreach(item => println(item))
        
    }
      
      reactions += {
        case e: MouseClicked => {
         // myListBuffer+= "New York"
         // listData = myListBuffer
        }
     //   case _: ListSelectionChanged(list, range, live) => myListBuffer+= "New York"
          
        // case LisSelectionChanged('myListView') => println(listView.selection.items(0))
        //case ButtonClicked(component) if component == button =>
      }
     
    }
      
      listenTo(myListView.selection)
 
      /*reactions += {
        case SelectionChanged(`myListView`) => {
          myListView.selection.items.headOption.foreach(item => {println(item)
            if(myListView.myListBuffer.contains(item))
              println("Contains: " + item)
              //myListView.myListBuffer-=item
            println("0000000000000")
           
            })
          
          
        }
      }*/
    

      /*contents += new ScrollPane(listView)

      val label = new Label("No selection")
      contents += label

      val b = new Button("Remove")
      contents += b

      listenTo(listView.selection, listView, b)
      reactions +=
        {
          case ListSelectionChanged(list, range, live) =>
            label.text = "Selection: " + range
          case e: ButtonClicked =>
            if (listView.listData.isEmpty)
            {
              b.enabled = false
            }
            else
            {
              listView.peer.getModel.asInstanceOf[DefaultListModel].remove(listView.selection.anchorIndex)
            }

          case ListElementsRemoved(source, range) =>
            println("Element at " + (range.start + 1) + " is removed.")
        }
    }*/
    
    
    
    
    /*contents = new BorderPanel {
      layout(getItemPanel) = North
      layout(itemsPanel0) = West
      layout(myListView) = Center
      layout(toggle) = East
      layout(textField) = South
    }
    size = new Dimension(700, 700)
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }*/

  //  preferredSize=new Dimension(500,500)
    
    
    /*
    // specify which Components produce events of interest
    listenTo(button)
    listenTo(toggle)
    listenTo(canvas.mouse.clicks)
    listenTo(btnGetItem0)

    // react to events
    reactions += {
      case ButtonClicked(component) if component == button =>
        val x = Random.nextInt(100)
        val y = Random.nextInt(100)
        val c = new Color(Random.nextInt(Int.MaxValue))
        canvas.throwDart(new Dart(x, y, c))
        textField.text = s"Dart thrown at $x, $y"
      case ButtonClicked(component) if component == toggle =>
        toggle.text = if (toggle.selected) "On" else "Off"
      
      case ButtonClicked(component) if component == btnGetItem0 =>
        txtItemNum0.text=""
        //add text to list and button
        toggle.text = if (toggle.selected) "On" else "Off"
      case MouseClicked(_, point, _, _, _) =>
        canvas.throwDart(new Dart(point.x, point.y, Color.black))
        textField.text = (s"You clicked in the Canvas at x=${point.x}, y=${point.y}.") 
    }*/
  }
}



/*object main extends App
{ 
  var exit = false
  println("Wprowadz liczbe agentow do utworzenia:")
  var inputAgents = readInt  
  // utworzenie N agentow i przechowywanie ich w liscie
  val AgentList = new ListBuffer[Robot]()
  
  for(x <- 1 to inputAgents)
  {
    AgentList += new Robot(x, "Robot " + x)
  }
  
  // uruchomienie wszystkich utworzonych agentow
  for(agent <- AgentList)
  {
    agent.start()
  }
  
  val m = new Master(AgentList)
  m.start()
  /*
  while(!exit)
  {
    
    exit = true
  }*/
  m.getProductsId()
  println("KONIEC")
  
}*/
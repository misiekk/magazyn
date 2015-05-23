package sag

import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import java.awt.{ Graphics2D, Color }
import ListView._
import swing._
import swing.event._
import GridBagPanel._
import java.awt.Insets
import scala.collection
import javax.swing.SwingConstants
//import java.io._



object main extends SimpleSwingApplication{
  
  //logic
  //var exit = false
  var inputAgents=0
  val AgentList = new ListBuffer[Robot]()

  
  def top = new MainFrame { // top is a required method
    title = "Magazyn"
    this.peer.setLocationRelativeTo(null)
    this.peer.setPreferredSize(new Dimension(800,600))
    
    // declare Components here
    val canvas=new Canvas(Warehouse.shelves)
    canvas.setTheSize(Warehouse.x_len_param, Warehouse.y_len_param)
  
    val ui=new GridBagPanel{
      val c=new Constraints
       
      val lblItem= new Label {text="Enter item number:" }
      val txtItemNum=new TextField
      val btnGetItem=new Button { 
        text="Get item" 
        enabled=false}
      val getItemPanel= new GridPanel(1,3){
        contents += lblItem
        contents += txtItemNum
        contents += btnGetItem
        hGap=10
      }  
      c.gridx = 0
      c.gridy = 0
      c.gridwidth=3
      c.gridheight=1
      c.weightx = 0
      c.weighty=0.05    
      layout(getItemPanel) = c
              
      val lblItems=new Label{
        text="Items:"
     //   horizontalAlignment=Alignment.Left  
     //   xAlignment=Alignment.Left
      //  preferredSize.width=300
        
        }
      val txtItems=new TextArea{
        for(item <- Warehouse.items){
          append("  " + item.ID + "      " + item.getStatus() + "   \n")
        }
        
      }
      val itemsScrollPanel = new ScrollPane(txtItems)    
      val itemsPanel=new BoxPanel(Orientation.Vertical){
        
        contents+=lblItems
        contents+=itemsScrollPanel
        background=Color.PINK 
      }   
      c.fill = Fill.Both
      c.gridx = 0
      c.gridy = 1
      c.gridwidth=1
      c.gridheight=2
      c.weightx = 0.15
      c.weighty=0.95   
      layout(itemsPanel)=c
        
      c.gridx = 1
      c.gridy = 1
      c.gridwidth=1
      c.gridheight=2
      c.weightx = 0.75
      c.weighty=0.95 
      layout(canvas)=c
      
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
      c.weightx = 0
      c.weighty=0.1
      layout(getAgentsNumPanel)=c
      
      c.gridx = 2
      c.gridy = 2
      c.gridwidth=1
      c.gridheight=1
      c.weightx = 0.125
      c.weighty=0.85
      layout(txtStats)=c
             
      def refreshItemList(){
        txtItems.text=""
        for(item <- Warehouse.items){
          txtItems.append("  " + item.ID + "      " + item.getStatus() + "   \n")
        }
      }
      listenTo(btnGetItem)
      listenTo(btnGetAgentsNum)
      listenTo(this)
      reactions+={
        //agentsNum defined
        case ButtonClicked(component) if component == btnGetAgentsNum =>      
          if (txtAgentsNum.text!=""){
            btnGetAgentsNum.enabled=false
            btnGetItem.enabled=true
            Warehouse.agentsNum=txtAgentsNum.text.toInt  
            txtStats.append("Number of robots is: " + txtAgentsNum.text)
            txtAgentsNum.text=""       
            //start agent logic
            Warehouse.initAgents()
            Warehouse.initMaster()
            canvas.repaint()
            
          }
          
        //item requested:
        case ButtonClicked(component) if component == btnGetItem =>
          var item=Warehouse.getItem(txtItemNum.text)
          if(item!=null)
            if(item.status==Status.InStore)
            {
              item.changeStatus(Status.AwaitingPickup)
              Warehouse.master.checkProductStatusFromList()
            }
          refreshItemList()
          txtItemNum.text=""
          
        case UIElementResized(_) => 
          canvas.resizeParams()
          
         
      }
    }
    contents=ui
    pack()
                
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
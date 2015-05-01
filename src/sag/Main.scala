package sag

import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer


object main extends App
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
  
}
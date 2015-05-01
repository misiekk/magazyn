package sag

import scala.actors.Actor
import scala.actors._
import scala.collection.mutable.ListBuffer


case object Hello



/* 
 * Klasa Mastera - glownego agenta
 * @robots - wszystkie podlegle agenty
 */
class Master(robots: ListBuffer[Robot]) extends Actor
{
	var startCheck : Int = 0


			def act()
{
	for (r <- robots) 
	{
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
}
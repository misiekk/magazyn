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
  def act()
  {
    for (r <- robots) r ! Hello  
  }
}
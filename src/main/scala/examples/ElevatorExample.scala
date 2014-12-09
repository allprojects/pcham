package examples

import pack1._

/**
 * example of PCHAM using an elevator over three different level
 */
object ElevatorExample extends App {
  
  //the current level of the elevator 
  val level1 = new Event("Level 1")
  val level2 = new Event("Level 2")
  val level3 = new Event("Level 3")
  
  //actions taken
  val up = new Event("up");
  val down = new Event("down");
  
  //Site handling events on Level 1
  Site(List(
      Handler(PRE{level1}, List(up))(POST{level2}, List()){println("Elevator goes from Level 1 up to Level 2")}
      ))(List()){println("Site 1 triggered")}
  
  //Site handling events on Level 2
  Site(List(
      Handler(PRE{level2}, List(up))(POST{level3}, List()){println("Elevator goes from Level 2 up to Level 3")},
      Handler(PRE{level2}, List(down))(POST{level1}, List()){println("Elevator goes from Level 2 down to Level 1")}
      ))(List()){println("Site 2 triggered")}
  
  //Site handling events on Level 3
  Site(List(
      Handler(PRE{level3}, List(down))(POST{level2}, List()){println("Elevator goes from Level 3 down to Level 2")}
      ))(List()){println("Site 3 triggered")}

  
  //start on Level 1
  level1!
  
  
  //let the elevator go crazy
  up! 
  
  up!
  
//  down!
//  
//  up!
//  
//  down!
//  
//  down!
//  
//  down!
//  
//  up!
  
  
  //lets see what happens!
  Monitor.run
  
  
}
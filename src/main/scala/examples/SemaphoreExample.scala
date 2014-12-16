package examples

import pack1._


/**
 * a simple example showing the implementation of a semaphore using the 
 * PCHAM 
 */
object SemaphoreExample extends App {
  
  //the states
  val green = new Event("State green");
  val yellow = new Event("State yellow");
  val red = new Event("State red");
  
  //the actions
  val up = new Event("action up");
  val down = new Event("action down");
  
  //Site handling events if state is red
  Site(List(
      Handler(PRE{red}, List(up))(POST{yellow }, List()){println("State gone up from red to yellow")},
      Handler(PRE{red}, List(down))(POST{red}, List()){println("State gone up from red, stayed red")}
      ))(List()){println("Site red triggered")}
  
  //Site handling events if state is yellow
  Site(List(
      Handler(PRE{yellow }, List(up))(POST{green}, List()){println("State gone up from yellow to green")},
      Handler(PRE{yellow }, List(down))(POST{red}, List()){println("State gone down from yellow to red")}
      ))(List()){println("Site yellow triggered")}
  
  //Site handling events if state is green
  Site(List(
      Handler(PRE{green}, List(up))(POST{green}, List()){println("State gone up from green, stayed green")},
      Handler(PRE{green}, List(down))(POST{yellow }, List()){println("State gone down from green to yellow")}
      ))(List()){println("Site green triggered")}

  
  //start in the green state
  green!
  
  //lets make something happen
  up!
  
  down!
  
  down!
  
  up!
  
  down!
  
  up!
  
  //run the thing
  Monitor.run
}
package examples.basic

import pack1._

/**
 * example of PCHAM using an elevator over three different level
 */
object ElevatorExample extends App {
  
  //the current level of the elevator
  //states are always private
  //we don't wanna outsiders make a mess out of it
  //e.g. be simultaneously in multiple states
  private val level1 = new Event("Level 1")
  private val level2 = new Event("Level 2")
  private val level3 = new Event("Level 3")
  
  //events to convert the global up and down events
  //to the corresponding local event
  //also private same as with the state
  private val not1 = new Event("not1")
  private val not2 = new Event("not2")
  private val not3 = new Event("not3")
  
  //actions taken
  //the only public events
  val up = new Event("up");
  val down = new Event("down");
  
  //the initial event to start the converter site
  val run = new Event("run");
  //event to initialize the elevator
  val init = new Event("init");
  
  
  //private actions used in the sites
  //again private or someone could mess up pretty baldy
  private val up1 = new Event("up1");
  private val down1 = new Event("down1");
  private val up2 = new Event("up2");
  private val down2 = new Event("down2");
  private val up3 = new Event("up3");
  private val down3 = new Event("down3");
  
  //silence the Monitor to better see what happens
  Monitor.mute
  
  //site to initialize the elevator
  Site(List(
      Handler(PRE{init}, List())(POST{level1}, List(run, not1)){println("initialized to Level 1")},
      Handler(PRE{init}, List())(POST{level2}, List(run, not2)){println("initialized to Level 2")},
      Handler(PRE{init}, List())(POST{level3}, List(run, not3)){println("initialized to Level 3")}
      ))(List()){println("Elevator wars initialized")}
  
  
  //site to convert public to correct private events
  Site(List(
      Handler(PRE{run}, List(up, not1))(POST{run}, List(up1)){println("up converted to up1")},
      Handler(PRE{run}, List(up, not2))(POST{run}, List(up2)){println("up converted to up2")},
      Handler(PRE{run}, List(up, not3))(POST{run}, List(up3)){println("up converted to up3")},
      Handler(PRE{run}, List(down, not1))(POST{run}, List(down1)){println("down converted to down1")},
      Handler(PRE{run}, List(down, not2))(POST{run}, List(down2)){println("down converted to down2")},
      Handler(PRE{run}, List(down, not3))(POST{run}, List(down3)){println("down converted to down3")}
      ))(List()){println("Converter Site triggered")}
  
  
  //Site handling events on Level 1
  //be careful to also create the converter events... 
  Site(List(
      Handler(PRE{level1}, List(up1))(POST{level2}, List(not2)){println("Elevator goes from Level 1 up to Level 2")},
      Handler(PRE{level1}, List(down1))(POST{level1}, List(not1)){println("Elevator tries going dowm from Level 1 down to Level 0,there is no Level 0")}
      ))(List()){println("Site 1 triggered")}
  
  //Site handling events on Level 2
  Site(List(
      Handler(PRE{level2}, List(up2))(POST{level3}, List(not3)){println("Elevator goes from Level 2 up to Level 3")},
      Handler(PRE{level2}, List(down2))(POST{level1}, List(not1)){println("Elevator goes from Level 2 down to Level 1")}
      ))(List()){println("Site 2 triggered")}
  
  //Site handling events on Level 3
  Site(List(
      Handler(PRE{level3}, List(down3))(POST{level2}, List(not2)){println("Elevator goes from Level 3 down to Level 2")},
      Handler(PRE{level3}, List(down3))(POST{level3}, List(not3)){println("Elevator tries going dowm from Level 3 up to Level 4,there is no Level 4")}
      ))(List()){println("Site 3 triggered")}

  //keep the Monitor silent
  //Monitor.mute
  
  //start on Level 1
  init!
  
  //let the elevator go crazy
  up! 
  
  
  up!
  
  down!
  
  up!
  
  down!
  
  down!
  
  down!
  
  up!
  
  
  
  
  //so we're done
  Monitor.finish
}
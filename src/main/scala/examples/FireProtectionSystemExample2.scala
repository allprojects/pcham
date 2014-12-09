package examples

import pack1._

/**
 * example of a simple Fire Protection System describing the use of
 * and the correct way of composing Handlers
 */
object FireProtectionSystemExample2 extends App {
  
  val init = new Event("init");

  //declare needed events
  val fireAlarm = new Event("fireAlarm"); 
  val smokeDetected = new Event("smokeDetected");
  val heatDetected = new Event("heatDetected");

  val releaseGas = new Event("releaseGas");
  val releaseAgent = new Event("releaseAgent");
  
  Site(List( // Handlers
        Handler(PRE{fireAlarm },List(smokeDetected))(POST{releaseGas},List()){println("Gas will be released")},
        // Handler(PRE{init },List(smokeDetected))(POST{releaseAgent},List()){println("Agent is being released")}, 
        // the above would cause an InvalidArgument Exception as a site may only compose Handlers with the same precondition, try it out!
        Handler(PRE{fireAlarm },List(heatDetected))(POST{releaseAgent},List()){println("Agent will be released")}        
        ))(List(fireAlarm.v)){}//the fireAlarm was already triggered
  
  smokeDetected! //fire event
  
  Monitor.run
  
}
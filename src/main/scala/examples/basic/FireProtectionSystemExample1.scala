package examples.basic

import pack1._

/**
 * example of a simple Fire Protection System describing the use of sites
 * and adding initial events to those
 */
object FireProtectionSystemExample1 extends App {
  
  val smokeDetected = new Event("smokeDetected");
  val heatDetected = new Event("heatDetected");
  
  val light = new Event("light");
  val horn = new Event("horn");
  
  Site(List( // Handlers
        Handler(PRE{Event.bottom},List(smokeDetected, heatDetected  ))(POST{Event.bottom },List(light , horn )){println("Lights and Horns are turned on")}
        ))(List(smokeDetected.v)){
  }
  
  //Event.bottom! doesn't need to be called as the bottom event is always 'fired' e.g. always present
  
  heatDetected!
  
  Monitor.run
}
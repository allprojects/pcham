package examples.basic

import pack1._

/**
 * example of a simple Fire Protection System showing the use of
 * the non determinism of a site
 */
object FireProtectionSystemExample3 extends App {

  //declare needed events
  val fireAlarm = new Event("fireAlarm"); 
  val smokeDetected = new Event("smokeDetected");

  val relGastoRedFuel = new Event("releaseGas");
  val relGastoRedHeat = new Event("relGastoRedHeat");
  val relGastoRedOxygen = new Event("relGastoRedOxygen");
  val relGastoRedChainReac = new Event("relGastoRedChainReac");
  
  
  Site(List( // all Handlers have the same reactants, the one to fire will be chosen non deterministically
        Handler(PRE{fireAlarm },List(smokeDetected))(POST{Event.bottom},List(relGastoRedFuel )){println("reduction of fuel")},
        Handler(PRE{fireAlarm },List(smokeDetected))(POST{Event.bottom},List(relGastoRedHeat )){println("reduction of heat")},
        Handler(PRE{fireAlarm },List(smokeDetected))(POST{Event.bottom},List(relGastoRedOxygen )){println("reduction of oxygen")},
        Handler(PRE{fireAlarm },List(smokeDetected))(POST{Event.bottom},List(relGastoRedChainReac)){println("reducing the chain reaction")}
        ))(List(fireAlarm.v)){}//the fireAlarm was already triggered
  //try running it multiple times, the triggering handler is random!
  
  
  smokeDetected! //fire event
  
  Monitor.finish
}
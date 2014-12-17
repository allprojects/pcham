package examples.agents.tokenring

import pack1._
import scala.compat.Platform

/**
 * An agent of the 'ping pong game'
 */
class Agent(val name: String) {

  var successor: Agent = null

  val receive = new Event(name + " recieve")
  val send = new Event(name + " send")

  //site to receive a message
  var receiveSite = Site(List(
    Handler(PRE(receive), List())(POST(send), List()) {}))(List()) {}

  //site defines logic
  //if it has received something and a tick occurs it has to send something 
  var sendSite = Site(List(
    Handler(PRE(send), List(Tokenring.tick))(POST(receive), List()) {}))(List()) {}
  //Sites may be combined into one 
  
  
  
  //set the neighbors of the agent and register the sites!  
  def set_neigh(arg1: Agent) = {
    successor = arg1

    Monitor.unregisterSite(receiveSite)
    Monitor.unregisterSite(sendSite)

    //site to receive a message
    receiveSite = Site(List(
      Handler(PRE(receive), List())(POST(send), List()) {}))(List()) { println(name + " received " + Tokenring.value) }

    //site defines logic
    //if it has received something and a tick occurs it has to send something 
    sendSite = Site(List(
      Handler(PRE(send), List(Tokenring.tick))(POST(successor.receive), List()) { println(name + " send to " + successor.name) }))(List()) {}
  }

}

//ugly construct to exchange the value between objects as OCHAM events
//can not carry any values
object Tokenring {
  private var current = 0
  def value = { current += 1; current }
  val tick = new Event("tick")
  println(value);
}

class Tokenring(val a1: Agent, val a2: Agent, val a3: Agent) {
  a1.set_neigh(a2)
  a2.set_neigh(a3)
  a3.set_neigh(a1)

  def progress() = Tokenring.tick!
}

object Tokenring_st extends App {

  var sun = new Agent("sun")
  var moon = new Agent("moon")
  var earth = new Agent("earth")

  var pingpong = new Tokenring(earth, moon, sun);
  earth.send!
  
  Monitor.verbose =false

  
  println("tokenring")
  val time: Long = Platform.currentTime
  while (Platform.currentTime - time < 10 * 1000) {
    pingpong.progress()
    println("progress");
    Thread.sleep(600)
  }

  

  Monitor.run
}


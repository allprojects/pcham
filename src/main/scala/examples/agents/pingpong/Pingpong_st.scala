package examples.agents.pingpong
import pack1._
import scala.compat.Platform

/**
 * An agent of the 'ping pong game'
 */
class Agent(val name: String) {

  var n1: Agent = null
  var n2: Agent = null

  val receive = new Event(name + " recieve")
  val send = new Event(name + " send")

  //site to receive a message
  var receiveSite = Site(List(
    Handler(PRE(receive), List())(POST(send), List()) {}))(List()) {}

  //site defines logic
  //if it has received something and a tick occurs it has to send something 
  var sendSite = Site(List(
    Handler(PRE(send), List(PingPong.tick))(POST(receive), List()) {},
    Handler(PRE(send), List(PingPong.tick))(POST(receive), List()) {}))(List()) {}

  //set the neighbors of the agent and register the sites!  
  def set_neigh(arg1: Agent, arg2: Agent) = {
    n1 = arg1
    n2 = arg2

    Monitor.unregisterSite(receiveSite)
    Monitor.unregisterSite(sendSite)

    //site to receive a message
    receiveSite = Site(List(
      Handler(PRE(receive), List())(POST(send), List()) {}))(List()) { println(name + " received " + PingPong.value) }

    //site defines logic
    //if it has received something and a tick occurs it has to send something 
    sendSite = Site(List(
      Handler(PRE(send), List(PingPong.tick))(POST(n1.receive), List()) { println(name + " send to " + n1.name) },
      Handler(PRE(send), List(PingPong.tick))(POST(n2.receive), List()) { println(name + " send to " + n2.name) }))(List()) {}
  }

}

//ugly construct to exchange the value between objects as OCHAM events
//can not carry any values
object PingPong {
  private var current = 0
  def value = { current += 1; current }
  val tick = new Event("tick")
  println(value);
}

class Pingpong(val a1: Agent, val a2: Agent, val a3: Agent) {
  a1.set_neigh(a3, a2)
  a2.set_neigh(a1, a3)
  a3.set_neigh(a2, a1)

  def progress() = PingPong.tick!
}

object Pingpong_st extends App {

  var sun = new Agent("sun")
  var moon = new Agent("moon")
  var earth = new Agent("earth")

  var pingpong = new Pingpong(earth, moon, sun);
  earth.send!
  
  Monitor.print =true

  
  println("pingpong cycle")
  val time: Long = Platform.currentTime
  while (Platform.currentTime - time < 10 * 1000) {
    pingpong.progress()
    println("progress");
    Thread.sleep(600)
  }

  

  Monitor.run
}


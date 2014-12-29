package pack1

import scala.collection.mutable._
import scala.collection._
import scala.util.Random
import scala.async.Async.{ async, await }
import scala.concurrent.Future
import scala.actors.threadpool.Executors
import scala.concurrent.ExecutionContext
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer

/**
 * Main component of the PCHAM implementation e.g. the 'Solution' holding all the sites
 * and responsible for disturbing the free events to the sites
 */
object Monitor {

  var reactions = new java.util.concurrent.atomic.AtomicInteger(0)

  val monitorThread: Thread = new Thread() {

    override def run() = {

      while (!this.isInterrupted()) {
        //check if some site triggered
        waitobj.synchronized {
          if (reactions.get() == 0) {
            if (finished) {
              interrupt()
            } else {
              waitobj.wait()
            }
          }
          reactions.set(0)
        }

        if (print) printSep("DISTRIBUTING EVENTS")
        distributeEscapedEvents()
        if (print) printlAll

        if (print) printSep("DOING LOCAL MATCH")
        sites.foreach { x =>
          {
            if (x.localMatch)
              reactions.incrementAndGet()
          }
        }
        if (print) printlAll

        if (print) printSep("Collecting EVENTS")
        collectEscapedEvents
        if (print) printlAll
      }
    }
  }

  var finished: Boolean = false
  //tells the monitor that the programm has ended and it can shout down ones it
  //computed all results
  def finish() = {
    finished = true;
    waitobj.synchronized {
      waitobj.notifyAll()
    }
  }

  //decide whether Monitor should print its state and result
  var print: Boolean = true
  def mute() = print = false
  def verbose() = print = true
  monitorThread.start()

  val sites = ListBuffer[Site]()
  def registerSite(s: Site) = s +=: sites
  def unregisterSite(s: Site) = sites -= s

  //list holding all free/escaped events 
  var escapedEvents = ListBuffer[Event]()
  def addEscapedEvent(e: Event) = e +=: escapedEvents

  //method to collect free events from all present sites
  def collectEscapedEvents = escapedEvents ++= sites.flatMap(_.getEscapingEvents)

  /**
   * filter function for sites returns true if a handler of the given
   * site needs the given event as one of his reactants
   */
  def filter(e: Event, s: Site): Boolean = {
    for (h <- s.handlers) {
      if (h.reactants.contains(e))
        return true //add event only to site that needs it
    }
    return false
  }

  // Assigns spare events to sites. Play with different strategies
  def distributeEscapedEvents() = {
    //if(!escapedEvents.contains(Event.bottom))escapedEvents.insert(0, Event.bottom)
    escapedEvents.foreach { e =>
      val filteresSites = sites.filter(filter(e, _))

      if (filteresSites.size > 0) {
        val randomSite = filteresSites(Random.nextInt(filteresSites.size));
        randomSite.addProduct(e)
        escapedEvents.remove(escapedEvents.indexOf(e))
      }
    }
  }

  val waitobj = new Object
  /**
   * Main method that starts the Monitor
   */
  def run() = for (i <- 0 to 3) {

    if (print) printSep("DISTRIBUTING EVENTS")
    distributeEscapedEvents()
    if (print) printlAll

    if (print) printSep("DOING LOCAL MATCH")
    sites.foreach { _.localMatch }
    if (print) printlAll

    if (print) printSep("Collecting EVENTS")
    collectEscapedEvents
    if (print) printlAll
  }

  def printSep(s: String) = println("--" + s + "---------------------------------")
  def printlAll = {
    sites.foreach { println(_) }
    escapedEvents.foreach { println(_) }
  }
}

/**
 * class describing an event
 */
class Event(name: String) {
  def ! = {
    Monitor.addEscapedEvent(this)
    new EventValue(this)
    Monitor.waitobj.synchronized {
      Monitor.waitobj.notifyAll()
    }
  }

  def v = {
    new EventValue(this)
  }
  override def toString = name
}
class EventValue(val e: Event)

/**
 * adds events constants e.g. the Bottom event always present in every Site
 */
object Event {
  val bottom = new Event("bottom");
}

/**
 * class describing a single Handler
 */
class Handler(val reactants: List[Event])(val products: List[Event])(body: => Unit) {
  /**
   * execute the body of the handler => fire the handler
   */
  def ! = {
    body
  }

  //TODO
  override def toString = "HANDLER: " +
    "[" + reactants(0) + "]" +
    reactants.takeRight(reactants.size - 1).toString() + " ᐅ  " +
    "[" + products(0) + "]" +
    products.takeRight(products.size - 1).toString

}
object Handler {
  def apply(pre: PRE, reactants: List[Event])(post: POST, products: List[Event])(body: => Unit) = new Handler(pre.pre :: reactants)(post.post :: products)(body)
  def apply(reactants: List[Event])(post: POST, products: List[Event])(body: => Unit) = new Handler(Event.bottom :: reactants)(post.post :: products)(body)
}

/**
 * class describing a Site that may hold an arbitrary number of Handlers and initially present events
 * if a handler of the Site executes the body of the SIte is executed
 * site is automatically added to the monitor
 *
 */
class Site(val handlers: List[Handler])(initialProducts: List[EventValue])(body: => Unit) {

  //initial products e.g. initially present events are all given events
  var products: List[Event] = initialProducts.map(_.e)

  if (Monitor.print) println("declaration: " + this.toString())

  Monitor.registerSite(this)

  //check whether all Handlers have the same precondition
  // as a site can't be composed of handlers with different preconditions
    {
      val evt = handlers(0).reactants(0)
      handlers.foreach(x => if (x.reactants(0) != evt)
        throw new IllegalArgumentException("A Site may only compose Handlers with the same precondition Handler \n" + this))
    }

  def addProduct(p: Event) = {
    this.synchronized {
      val s = p :: products
      products = s
    }
  }

  // The products that do not appear in any of the handlers as reactants 
  def getEscapingEvents = {
    var escaping = products.filter(x => !handlers.flatMap(_.reactants).contains(x)) //collect events not bound by a handler
    products = products.filter(!escaping.contains(_)) //remove the escaping events from site
    escaping //return the escaping events as a result
  }

  // Does not manage duplicate events!
  def getFiringHandlers = handlers.filter(handlerIsTriggered(_))

  private def handlerIsTriggered(handler: Handler): Boolean = {
    val react = handler.reactants.to[ListBuffer]
    products.foreach(x => {
      if (react.contains(x))
        react.remove(react.indexOf(x))

      if (react.isEmpty)
        return true
    })
    return false
  }

  // randomly choose which handler should fire
  def chooseFiringHandler(hs: List[Handler]) = hs(Random.nextInt(hs.size))

  override def toString = "SITE[ " + handlers.toString() +
    "\n                     ?  " + products.toString + " ]"

  // In a separate thread ?
  def localMatch: Boolean = {
    val fs = getFiringHandlers // Select the handlers that fire 
    if (!fs.isEmpty) { // At least one handler is firing, else return 
      body // Execute body
      val firingHandler = chooseFiringHandler(fs) // Chose one handler if many are firing
      firingHandler! //execute the body of the firing Handler //TODO
      val reactants = firingHandler.reactants
      //bug, can't handle multiple occurences of a event type and negation missing
      //TODO fix the bug... sometime... properly 
      val react = reactants.to[ListBuffer]
      products = products.filter(x => {
        if (!react.contains(x)) {
          true
        } else {
          react.remove(react.indexOf(x))
          false
        }
      }) // Remove events that match      
      products = products ::: firingHandler.products // Add products
      return true
    }
    return false
  }
}
object Site {
  def apply(handlers: List[Handler])(initialProducts: List[EventValue])(body: => Unit) = new Site(handlers)(initialProducts)(body)
}

/**
 * Wrapper for an event describing a precondition
 */
class PRE(val pre: Event)
//syntactic sugar for a precondition
object PRE {
  def apply(pre: Event) = new PRE(pre)
}

/**
 * Wrapper for an describing a postcondition
 */
class POST(val post: Event)
//syntactic sugar for a postcondition 
object POST {
  def apply(post: Event) = new POST(post)
}

/**
 * simple example of the use of the PCHAM in form of a fire protection system
 */
object FireProtectionSystemExample extends App {

  //define all events used by the system
  val init = new Event("init");
  val firealarm = new Event("firealarm");

  val smokeDetected: Event = new Event("smokeDetected")
  val heatDetected: Event = new Event("heatDetected")
  val light: Event = new Event("light")
  val horn: Event = new Event("horn")

  val dummyEvent: Event = new Event("dummyEvent")

  //define a site 
  val s = Site(List( // Handlers
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any one") },
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any two") }
    ))(List()) { // Inital reactants
      // Semantics: the code in the body is executed after the match and before creating the products
      println("Site executed!")
    }

  // if (file empty){
  //    ...
  //    light!
  //  }

  //fire the events
  init!

  smokeDetected!

  heatDetected!

  //tell the monitor no more events will arrive
  //else it runs infinitely
  Monitor.finish
}







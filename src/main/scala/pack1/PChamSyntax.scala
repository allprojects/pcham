package pack1

import scala.collection.mutable._
import scala.collection._
import scala.util.Random


/**
 * Main component of the PCHAM implementation e.g. the 'Solution' holding all the sites
 * and responsible for disturbing the free events to the sites
 */
object Monitor {
  
  val sites = ListBuffer[Site]()
  def registerSite(s: Site) = s +=: sites  
  
  //list holding all free/escaped events 
  var escapedEvents = ListBuffer[Event]()
  def addEscapedEvent(e: Event) = e +=: escapedEvents
  
  //method to collect free events from all present sites
  def collectEscapedEvents = escapedEvents ++= sites.flatMap(_.getEscapingEvents) 
  
  // Assigns spare events to sites. Play with different strategies
  def distributeEscapedEvents() = { 
    escapedEvents.foreach{e=>
      val randomSite = sites(Random.nextInt(sites.size))
      randomSite.addProduct(e) 
    }
    escapedEvents = ListBuffer[Event]()
  }  
  
  
  /**
   * Main method that starts the Monitor
   */
  def run = for (i <- 1 to 3) {
    
    printSep("DOING LOCAL MATCH")
    sites.foreach{_.localMatch}
    printlAll
    
    printSep("Collecting EVENTS")
    collectEscapedEvents
    printlAll
    
    printSep("DISTRIBUTING EVENTS")
    distributeEscapedEvents()
    printlAll
  }
  
  def printSep(s: String) =  println("--" + s + "---------------------------------")
  def printlAll = {
    sites.foreach{println(_)}
    escapedEvents.foreach{println(_)}
  }
}


/**
 * class describing an event
 */
class Event(name: String) {
  def ! = {
    Monitor.addEscapedEvent(this)
    new EventValue(this)
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
object Event{
  val bottom = new Event("bottom");
}


/**
 * class describing a single Handler
 */
class Handler(val reactants: List[Event])(val products: List[Event])(body: => Unit){
  
  def ! ={
    body
  }
  
  override def toString = "HANDLER: " + 
    "[" + reactants(0) + "]" + 
    reactants.takeRight(reactants.size-1).toString() + " ᐅ  " +
    "[" + products(0) + "]" +
    products.takeRight(products.size-1).toString
  
}
object Handler {
  def apply(pre: PRE, reactants: List[Event])(post: POST, products: List[Event])(body: => Unit) = new Handler(pre.pre::reactants)(post.post::products)(body)
}


/**
 * class describing a Site that may hold an arbitrary number of Handlers and initially present events
 * if a handler of the Site executes the body of the SIte is executed
 * 
 */
class Site(val handlers: List[Handler])(initialProducts: List[EventValue])(body: =>Unit) {
  
  //initial products e.g. initially present events are all given events and the bottom event
  var products: List[Event] = Event.bottom :: initialProducts.map(_.e)
  
  println("declaration: " + this.toString())
  
  
  Monitor.registerSite(this)
  
  //check whether all Handlers have the same precondition
  {
    val evt = handlers(0).reactants(0)
    handlers.foreach( x => if(x.reactants(0) != evt) 
      throw new IllegalArgumentException("A Site may only compose Handlers with the same precondition Handler \n"+this))
  }
  //TODO Handler error, site can't be composed of handlers with different preconditions

  
  def addProduct(p: Event) = { val s = p :: products
    products = s
  }
  
  // The products that do not appear in any of the handlers as reactants 
  def getEscapingEvents = products.filter(x => !handlers.flatMap(_.reactants).contains(x))
          
  // Does not manage duplicate events!
  def getFiringHandlers = handlers.filter(x => x.reactants.toSet subsetOf products.toSet)
  
  // Dummy implementation
  def chooseFiringHandler(hs: List[Handler]) = hs(0)
  
  override def toString = "SITE[ " + handlers.toString() + 
                          "\n                     ?  " + products.toString + " ]"
  
  // In a separate thread ?
  def localMatch: Unit = {
    val fs = getFiringHandlers // Select the handlers that fire 
    if (!fs.isEmpty) { // At least one handler is firing, else return 
      body // Execute body
      val firingHandler = chooseFiringHandler(fs) // Chose one handler if many are firing
      firingHandler!	//execute the body of the firing Handler
      val reactants = firingHandler.reactants    
      products = products.filter(reactants.contains(_)) // Remove events that match
      products = if (products.contains(Event.bottom )) products else Event.bottom :: products //add bottom event again if it has been consumed 
      products = products ::: firingHandler.products // Add products
    }    
  } 
  
}
object Site {
  def apply(handlers: List[Handler])(initialProducts: List[EventValue])(body: =>Unit) = new Site(handlers)(initialProducts)(body)
}




/**
 * Wrapper for an event describing a precondition
 */
class PRE(val pre: Event)
//syntactic sugar for a precondition
object PRE{
  def apply(pre: Event) = new PRE(pre)
}


/**
 * Wrapper for an describing a postcondition
 */
class POST(val post: Event)
//syntactic sugar for a postcondition 
object POST{
  def apply(post: Event) = new POST(post)
}


/**
 * simple example of the use of the PCHAM in form of a fire protection system  
 */
object FireProtectionSystemExample extends App {
  
  
  
  //define all events used by the system
  val init = new Event("init");
  val firealarm = new Event("firealarm");
  
  val smokeDetected :Event = new Event("smokeDetected")
  val heatDetected :Event = new Event("heatDetected")
  val light :Event = new Event("light")
  val horn :Event = new Event("horn")
  
  val dummyEvent :Event = new Event("dummyEvent")

  
  
  //define a site 
  Site(List( // Handlers
        Handler(PRE{init},List(smokeDetected,heatDetected))(POST{firealarm},List(light,horn)){ println("any")},
        Handler(PRE{init },List(smokeDetected,heatDetected))(POST{firealarm},List(light,horn)){ println("any")}
        ))(List(smokeDetected.v, dummyEvent.v)){  // Inital reactants
    
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
  
  //start the monitor
  Monitor.run
  
  //no concurrence jet
  //events have to be fired before the run method is called...
  

}







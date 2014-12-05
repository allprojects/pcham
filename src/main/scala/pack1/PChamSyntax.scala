package pack1

import scala.collection.mutable._
import scala.collection._
import scala.util.Random


object Monitor {
  
  val sites = ListBuffer[Site]()
  def registerSite(s: Site) = s +=: sites  
  
  var escapedEvents = ListBuffer[Event]()
  def addEscapedEvent(e: Event) = e +=: escapedEvents
  
  def collectEscapedEvents = escapedEvents ++= sites.flatMap(_.getEscapingEvents) 
  
  // Assigns spare events to sites. Play with different strategies
  def distributeEscapedEvents() = { 
    escapedEvents.foreach{e=>
      val randomSite = sites(Random.nextInt(sites.size))
      randomSite.addProduct(e) 
    }
    escapedEvents = ListBuffer[Event]()
  }  
  
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



class Handler(val reactants: List[Event])(val products: List[Event])(body: =>Unit){
  
  override def toString = "HANDLER: " + 
    "[" + reactants(0) + "]" + 
    reactants.takeRight(reactants.size-1).toString() + " ?  " +
    "[" + products(0) + "]" +
    products.takeRight(products.size-1).toString
  
}
object Handler {
  def apply(pre: PRE, reactants: List[Event])(post: POST, products: List[Event])(body: => Unit) = new Handler(pre.pre::reactants)(post.post::products)(body)
}



class Site(val handlers: List[Handler])(initialProducts: List[EventValue])(body: =>Unit) {
  
  var products: List[Event] = initialProducts.map(_.e)
  
  println("declaration: " + this.toString())
  
  Monitor.registerSite(this)
  
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
      val reactants = firingHandler.reactants    
      products = products.filter(reactants.contains(_)) // Remove events that match
      products = products ::: firingHandler.products // Add products
    }    
  } 
  
}
object Site {
  def apply(handlers: List[Handler])(initialProducts: List[EventValue])(body: =>Unit) = new Site(handlers)(initialProducts)(body)
}


object PRE{
  def apply(pre: Event) = new PRE(pre)
}
class PRE(val pre: Event)

object POST{
  def apply(post: Event) = new POST(post)
}
class POST(val post: Event)



object FireProtectionSystemExample extends App {
  
  
  
  
  val init = new Event("init");
  val firealarm = new Event("firealarm");
  
  val smokeDetected :Event = new Event("smokeDetected")
  val heatDetected :Event = new Event("heatDetected")
  val light :Event = new Event("light")
  val horn :Event = new Event("horn")
  
  val dummyEvent :Event = new Event("dummyEvent")

  
  
   
  Site(List( // Handlers
        Handler(PRE{init},List(smokeDetected,heatDetected))(POST{firealarm},List(light,horn)){ println("any")}
        ))(List(smokeDetected.v, dummyEvent.v)){  // Inital reactants
    
    // Semantics: the code in the body is executed after the match and before creating the products
    
    println("Site executed!")
    
  }
  
  
  
  

  // if (file empty){
  //    ...
  //    light!
  //  }
  
  init!
  
  smokeDetected!
  
  heatDetected!

  
  Monitor.run
  

}







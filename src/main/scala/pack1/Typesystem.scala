package pack1

import scala.collection.mutable.Set


/**
 * the typesystem itself which may receive an program in the form of a List an tell whether the program is valid or not
 */
object Typesystem {

  /**
   * Method to validate an entire program
   * returns true if none of the rules of the calculus was violated
   * false otherwise
   */
  def validate(program: List[Any]): Boolean = {
    if (program.isEmpty)
      return true

    var res = false
    if (program.head.isInstanceOf[Site]) {
      res = validateSite(program.head.asInstanceOf[Site], program)
    } else if (program.head.isInstanceOf[Event]) {
      res = validateEvent(program.head.asInstanceOf[Event], program)
    }
    if (res)
      return validate(program.tail)
    else
      return false
  }

  /**
   * Method to validate a single site in the context of a given program
   */
  def validateSite(s: Site, program: List[Any]): Boolean = {
    var events = Set[Event]()
    for (h <- s.handlers)
      events = events ++ h.reactants ++ h.products

    for (e <- events)
      if (!validateEvent(e, program))
        return false

    return true
  }

  /**
   * Method to validate a single event in the context of a given program
   */
  def validateEvent(e: Event, program: List[Any]): Boolean = {
    var typecount = Array.fill[Int](Type.maxId+1)(0)
    
    //TODO implement
    for(element <- program){
      val etype = getTypeOf(e, element)
      typecount(etype.id)=typecount(etype.id)+1
    }

    return BasicRuleSet(typecount)
  }

  /**
   * Method to get the new type of an event if it has different types within a site
   */
  def newType(a: Type.Type, b: Type.Type): Type.Type = {
    (a, b) match {
      case (`b`, _) => a
      case (Type.g, Type.p) => Type.g
      case (Type.g, Type.linP) => Type.linG
      case (Type.linG, Type.linP) => Type.linG
      case (Type.g, Type.linR) => Type.linR
      case (Type.invalid, _) => Type.invalid
      case (_, Type.invalid) => Type.invalid
      case (Type.linR, _) => Type.invalid
      case (_, Type.linR) => Type.invalid
      case (Type.void, _) => b
      case (_, Type.void) => a
      case _ => a
    }
  }

  /**
   * returns the type of the given event in the context of s according to the pcham calculus
   */
  def getTypeOf(e: Event, s: Any): Type.Type = {
    s match {
      case e2: Event => getTypeOf(e, e2)
      case s2: Site => getTypeOf(e, s2)
      case h: Handler => getTypeOf(e, h)
      case _ => Type.g
    }
  }

  /**
   * returns the type of the given event within the given site
   */
  def getTypeOf(e: Event, s: Site): Type.Type = {
    var currType = if (s.handlers.size > 0) getTypeOf(e, s.handlers.head) else Type.void
    for (h <- s.handlers) {
      currType = newType(currType, getTypeOf(e, h))
    }
    currType
  }

  /**
   * returns the type of the given event within the given handler
   */
  def getTypeOf(e: Event, s: Handler): Type.Type = {
    if (s.reactants(0) == e)
      Type.linR
    else if (s.products(0) == e)
      Type.linP
    else if (s.reactants.contains(e))
      Type.r
    else if (s.products.contains(e))
      Type.p
    else
      Type.void
  }

  /**
   * returns the type of the given event within the given event
   *
   * note: only added for completeness e.g. to simplify the implementation
   */
  def getTypeOf(e: Event, s: Event): Type.Type = {
    if (e == s)
      Type.g
    else
      Type.void
  }

}

/**
 * the types an event can be categorized as by the typesystem
 */
object Type extends Enumeration {
  type Type = Value
  val linR, linP, linG, g, r, p, void, invalid = Value
}

trait Rule {
  def apply(types: Array[Int]): Boolean
}

object Rule {
  def apply(x: Array[Int] => Boolean) = new Rule() { def apply(types: Array[Int]): Boolean = x(types) }
}

object BasicRuleSet {
  val set: Set[Rule] = Set.empty

  def apply(x: Array[Int]): Boolean = {
    set.foreach(r => if (!r(x)) return false)
    return true
  }
}

object test extends App {
  //define all events used by the system
  val init = new Event("init");
  val firealarm = new Event("firealarm");

  val smokeDetected: Event = new Event("smokeDetected")
  val heatDetected: Event = new Event("heatDetected")
  val light: Event = new Event("light")
  val horn: Event = new Event("horn")

  val dummyEvent: Event = new Event("dummyEvent")

  Monitor.mute
  //define a site 
  val s = Site(List( // Handlers
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any one") },
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any two") },
    Handler(PRE { init }, List(smokeDetected, dummyEvent))(POST { firealarm }, List(light, horn)) { println("any three") }))(List()) { // Inital reactants
    // Semantics: the code in the body is executed after the match and before creating the products
    println("Site executed!")
  }

  
  
  val program = List(dummyEvent , s)
  println(Typesystem.validateEvent(dummyEvent , program))
  
  Monitor.finish
}
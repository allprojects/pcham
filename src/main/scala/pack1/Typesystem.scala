package pack1

import scala.collection.mutable.Set

/**
 * the typesystem itself which may receive an program in the form of a List an tell whether the program is valid or not
 */
object Typesystem {

  /**
   * Validate the entire program
   * returns true if none of the rules of the calculus was violated
   * false otherwise
   */
  def validate(program: List[Any]): Boolean = {
    if (program.isEmpty)
      return true

    var res = false
    program.head match {
      case e: Event => res = validateEvent(e, program)
      case s: Site => res = validateSite(s, program)
    }

    if (res)
      return validate(program.tail)
    else
      return false
  }

  /**
   * Validate a single site in the context of a given program
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
   * Validate a single event in the context of a given program
   */
  def validateEvent(e: Event, program: List[Any]): Boolean = {
    var typecount = Array.fill[Int](Type.maxId)(0)

    //TODO implement
    for (element <- program) {
      val etype = getTypeOf(e, element)
      typecount(etype.id) = typecount(etype.id) + 1
    }

    return BasicRuleSet(typecount, e)
  }

  /**
   * Compute the resulting type of an event that has multiple types within a site
   *
   * @param a the current type of the events in the site
   * @param b the new type it may have
   *
   * @return the resulting type of the event
   */
  def newType(a: Type.Type, b: Type.Type): Type.Type = {
    (a, b) match {
      case (`b`, _) => a //new type the same as before  
      case (Type.g, Type.p) => Type.g //product is global 
      case (Type.g, Type.linP) => Type.linG //postcondition is global
      case (Type.linG, Type.linP) => Type.linG
      case (Type.linG, Type.linR) => Type.invalid //previously global event may not be a precondition e.g. site has different events as preconditions 
      case (Type.linR, Type.void) => Type.invalid //new event may not become a precondition
      case (Type.linR, Type.g) => Type.linR //precondition may not leave site and stays precondition!
      case (Type.linR, Type.linG) => Type.linR
      case (Type.invalid, _) => Type.invalid //once invali always invalid
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
    if (s.products.contains(e)) {
      currType = newType(currType, Type.g)
    }
    currType
  }

  /**
   * returns the type of the given event within the given handler
   */
  def getTypeOf(e: Event, s: Handler): Type.Type = {
    if (s.reactants(0) == e)
      Type.linR
    else if (s.reactants.contains(e))
      Type.r
    else if (s.products(0) == e)
      Type.linP
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

/**
 * a rule that has to hold for the whole program
 * the array counts how often the currently checked
 * event occurs in the program as a specific type
 *
 * should return true if the rule holds e.g everything is alright
 * and false otherwise
 */
trait Rule {
  var rulename: String = ""
  def apply(types: Array[Int]): Boolean
}

object Rule {
  def apply(x: Array[Int] => Boolean)(name: String) = new Rule() { rulename = name; def apply(types: Array[Int]): Boolean = x(types) }
}

/**
 * object holding the basic rules of pcham
 */
object BasicRuleSet {
  val set: Set[Rule] = Set.empty

  //add rule: precondition may only used by one site
  set += Rule(x => x(Type.linR.id) < 2)("unique precondition(r1)")

  //add rule: precondition may not occur as a reactant and leave site
  set += Rule(x => !(x(Type.linR.id) > 0 && x(Type.r.id) > 0 && ((x(Type.g.id) > 0) || x(Type.linG.id) > 0 || x(Type.linP.id) > 0 || x(Type.p.id) > 0)))("deadlock free (r2-5)")

  //add rule: multiple occurrences as reactant
  set += Rule(x => !(x(Type.r.id) > 1 && ((x(Type.g.id) > 0) || x(Type.linG.id) > 0 || x(Type.linP.id) > 0 || x(Type.p.id) > 0)))("deadlock free (r8-11)")

  //add rule: preserver order
  set += Rule(x => !(x(Type.linR.id) > 0 && (x(Type.linP.id) > 0 || x(Type.linG.id) > 0) && (x(Type.p.id) > 0 || x(Type.g.id) > 0)))("order preservance (r12-15)")

  def apply(x: Array[Int], e: Event): Boolean = {
    set.foreach(r => if (!r(x)) { println("Rule : " + r.rulename + " does not hold for " + e); printArray(x); return false })
    return true
  }

  def printArray(x: Array[Int]) {
    for (i <- 0 to x.length - 1)
      println(Type.apply(i) + "	" + x(i))
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
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any one") }))(List()) { // Inital reactants
    // Semantics: the code in the body is executed after the match and before creating the products
    println("Site executed!")
  }

  //val program = List(dummyEvent , s)
  val program = List(dummyEvent, s, s) //would be categorized as invalid
  println(Typesystem.validateEvent(init, program))

  Monitor.finish
}
package pack1

object Typesystem {

  def validate(program: List[Any]): Boolean = {
    if (program.isEmpty)
      return true

    return validate(program.tail)
  }

  val blub = Type.linG

  def newType(a: Type.Type, b: Type.Type): Type.Type = {
    (a, b) match {
      case (_, Type.invalid) => Type.invalid
      case (Type.invalid, _) => Type.invalid
      case (_, Type.g) => a
      case (Type.g, _) => b
      case _ => Type.g
    }
  }

  def newTypeSite(a: Type.Type, b: Type.Type): Type.Type = {
    (a, b) match {
      case (`b`, _) => a
      case (Type.g, Type.p) => Type.g
      case (Type.g, Type.linP) => Type.linG
      case (Type.linG, Type.linP) => Type.linG
      case (Type.g , Type.linR) => Type.linR
      case (Type.invalid , _) => Type.invalid
      case (_ , Type.invalid) => Type.invalid      
      case (Type.linR, _) => Type.invalid
      case (_, Type.linR) => Type.invalid
      case (Type.void, _) => b
      case (_, Type.void) => a
      case _ => Type.void 
    }
  }

  def getTypeOf(e: Event, s: Any): Type.Type = {
    s match {
      case e2: Event => getTypeOf(e, e2)
      case s2: Site => getTypeOf(e, s2)
      case h: Handler => getTypeOf(e, h)
      case _ => Type.g
    }
  }

  def getTypeOf(e: Event, s: Site): Type.Type = {
    var currType = Type.g
    for (h <- s.handlers) {
      currType = newTypeSite(currType, getTypeOf(e, h))
    }
    currType
  }

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
      Type.g
  }

  def getTypeOf(e: Event, s: Event): Type.Type = {
    if (e == s)
      Type.g
    else
      Type.void
  }

}

object Type extends Enumeration {
  type Type = Value
  val linR, linP, linG, g, r, p, void, invalid = Value
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
    Handler(PRE { init }, List(smokeDetected, heatDetected))(POST { firealarm }, List(light, horn)) { println("any two") }))(List()) { // Inital reactants

    // Semantics: the code in the body is executed after the match and before creating the products

    println("Site executed!")
  }

  println("init " + Typesystem.getTypeOf(init, s))
  println("firealarm " + Typesystem.getTypeOf(firealarm, s))
  println("smokeDetected " + Typesystem.getTypeOf(smokeDetected, s))
  println("heatDetected " + Typesystem.getTypeOf(heatDetected, s))

  Monitor.finish
}

import org.junit.Test
import org.junit.Assert._
import pack1._

class RuleTests {

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

  @Test def uniquePreconditionHolds() {
    val program = List(dummyEvent, s) //would be categorized as valid
    assertTrue(Typesystem.validate(program))
  }

  @Test def uniquePreconditionFails() {
    val program = List(dummyEvent, s, s) //would be categorized as invalid
    assertFalse(Typesystem.validate(program))
  }

  
  
  @Test def reactantAndPreconditionHolds() {
    val initCycle = Site(List( // Handlers
      Handler(PRE { dummyEvent }, List(init))(POST { init }, List()) { println("any one") }))(List(init.v)) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, initCycle) //would be categorized as valid
    assertTrue(Typesystem.validate(program))
  }

  @Test def reactantAndPreconditionFails() {
    val initCycle = Site(List( // Handlers
      Handler(PRE { dummyEvent }, List(init))(POST { init }, List()) { println("any one") }))(List(init.v)) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, initCycle, init) //would be categorized as invalid
    assertFalse(Typesystem.validate(program))
  }

  
  
  @Test def reactantUniqueIfGlobalHolds() {
    val shutters: Event = new Event("shutters")
    val shutterClose = Site(List( // Handlers
      Handler(PRE { dummyEvent }, List(smokeDetected))(POST { shutters}, List(smokeDetected)) { println("any one") }))(List(dummyEvent .v)) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, shutterClose ) //would be categorized as valid
    assertTrue(Typesystem.validate(program))
  }

  @Test def reactantUniqueIfGlobalFails() {
    val shutters: Event = new Event("shutters")
    val shutterClose = Site(List( // Handlers
      Handler(PRE { dummyEvent }, List(smokeDetected))(POST { shutters}, List()) { println("any one") }))(List(dummyEvent .v)) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, shutterClose, smokeDetected ) //would be categorized as invalid
    assertFalse(Typesystem.validate(program))
  }
  
  
  @Test def orderIsPreservedHolds(){
    val shutters: Event = new Event("shutters")
    val shutterClose = Site(List( // Handlers
      Handler(PRE { firealarm  }, List())(POST { shutters}, List()) { println("any one") }))(List()) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, shutterClose ) //would be categorized as valid
    assertTrue(Typesystem.validate(program))
  }
  
  @Test def orderIsPreservedFails(){
    val shutters: Event = new Event("shutters")
    val shutterClose = Site(List( // Handlers
      Handler(PRE { firealarm  }, List())(POST { shutters}, List()) { println("any one") }))(List()) {
      println("Site executed!")
    }
    val program = List(dummyEvent, s, shutterClose , firealarm ) //would be categorized as valid
    assertFalse(Typesystem.validate(program))
  }
}


import collection.mutable.Stack
import org.scalatest._
import pack1._

class TestSpec extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
  
  
    "   " should "pop values in last-in-first-out order" in {
      val smokeDetected :Event = new Event("smokeDetected")
      val heatDetected :Event = new Event("heatDetected")
      val light :Event = new Event("light")
      val horn :Event = new Event("horn")
  
    val dummyEvent :Event = new Event("dummyEvent")


  
    //val fireAlarm = Handler(List(smokeDetected,heatDetected))(List(light,horn))
  
   // val fireAlarmSite = Site(List(fireAlarm))(List(smokeDetected!, dummyEvent!, heatDetected!)){ }
    
//    
//    val a = fireAlarmSite.getEscapingEvents
//    
//    val b = fireAlarmSite.handlers.flatMap(_.reactants)
    
//    println(b)

  }
}















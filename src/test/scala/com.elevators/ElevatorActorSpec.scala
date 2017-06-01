package com.elevators

import java.util.concurrent.Executors

import akka.actor.{ ActorRef, ActorSystem }
import akka.pattern.ask
import akka.testkit.{ ImplicitSender, TestKit, TestProbe }
import org.scalatest._
import Matchers._
import akka.util.Timeout

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._

class ElevatorActorSpec
    extends TestKit(ActorSystem("Test-Elevator-System"))
    with ImplicitSender
    with AsyncWordSpecLike
    with BeforeAndAfterAll {

  implicit val timeout = Timeout(5 seconds)

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An Elevator actor" when {
    "called" should {
      val listener = TestProbe("notification-listener")
      val elevator: ActorRef =
        system.actorOf(ElevatorActor.props(7, listener.ref))
      implicit val ec = system.dispatcher

      "collect the calling passenger and deliver to requested floor" in {
        elevator ! PassengerToCollect(1, Passenger(goingToFloor = 9))
        waitTillIdle(1 second, listener)

        (elevator ? ElevatorStateRequest)
          .mapTo[ElevatorState]
          .map(elevatorState => {
            elevatorState.collectFrom should be(empty)
            elevatorState.delivered should contain(Passenger(goingToFloor = 9))
          })
      }

      "collect the callin passenger and deliver to requested floor" in {
        val passenger1 = Passenger(goingToFloor = 9)
        val passenger2 = Passenger(goingToFloor = 2)
        val passenger3 = Passenger(goingToFloor = 3)
        elevator ! PassengerToCollect(1, passenger1)
        elevator ! PassengerToCollect(7, passenger2)
        elevator ! PassengerToCollect(5, passenger3)
        waitTillIdle(1 second, listener)
        (elevator ? ElevatorStateRequest)
          .mapTo[ElevatorState]
          .map(elevatorState => {
            elevatorState.collectFrom should be(empty)
            elevatorState.delivered should contain(passenger1)
            elevatorState.delivered should contain(passenger2)
            elevatorState.delivered should contain(passenger3)
          })
      }
    }
  }

  def waitTillIdle(timeout: Duration, listener: TestProbe): Any = {
    val idleReceived: PartialFunction[Any, Boolean] = {
      case Idle => true
      case _ => false
    }
    listener.fishForMessage(1 second)(idleReceived)
  }

}

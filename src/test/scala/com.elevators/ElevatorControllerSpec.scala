package com.elevators

import akka.actor.{ ActorRef, ActorSystem }
import akka.pattern.ask
import akka.testkit.{ ImplicitSender, TestKit, TestProbe }
import akka.util.Timeout
import org.scalatest._
import Matchers._

import scala.concurrent.Future
import scala.concurrent.duration._

class ElevatorControllerSpec
    extends TestKit(ActorSystem("Test-Elevator-System"))
    with ImplicitSender
    with AsyncWordSpecLike
    with BeforeAndAfterAll {

  implicit val timeout = Timeout(5 seconds)

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An Elevator Controller" when {
    "called" should {
      val listener = TestProbe("notification-listener")
      implicit val ec = system.dispatcher
      val elevatorController: ActorRef =
        system.actorOf(ElevatorController.props(3, 7, listener.ref, ec))

      "collect the calling passenger and deliver to requested floor" in {
        elevatorController ! PassengerToCollect(1, Passenger(goingToFloor = 9))
        waitTillIdle(1 second, listener)

        val elevatorStates: Future[Seq[ElevatorState]] =
          (elevatorController ? ElevatorStateRequest).mapTo[Seq[ElevatorState]]

        elevatorStates.map { states =>
          {
            states.flatMap(_.delivered) should contain(
              Passenger(goingToFloor = 9)
            )
            states.flatMap(_.collectFrom) should be(empty)
          }

        }
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

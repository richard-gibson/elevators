package com.elevators

import org.scalatest.{ Matchers, WordSpecLike }
import Matchers._

class BasicElevatorSpec extends WordSpecLike {

  object elevator extends ElevatorBehaviour

  "An elevators behaviour" must {
    "schedule a passenger to be collected" in {
      val passenger = Passenger(goingToFloor = 9)
      val toBeCollected =
        elevator.collectPassengerFrom(4, passenger, Set.empty)
      toBeCollected should contain((4, passenger))
    }

    "schedule a passenger to be left off" in {
      val passenger = Passenger(goingToFloor = 3)
      val origTakingTo =
        Set(Passenger(goingToFloor = 5), Passenger(goingToFloor = 1))
      val newTakingTo = elevator.takePassengerTo(passenger, origTakingTo)
      newTakingTo should contain(passenger)
    }

    "collect a passenger" in {
      val passenger = Passenger(goingToFloor = 9)
      val origTakingTo =
        Set(Passenger(goingToFloor = 5), Passenger(goingToFloor = 1))
      val origToBeCollected =
        Set((3, passenger), (5, Passenger(goingToFloor = 1)))
      val (newToBeCollected, newTakingTo) =
        elevator
          .passengerCollected(3, passenger, origToBeCollected, origTakingTo)

      newTakingTo should contain(passenger)
      newToBeCollected shouldNot contain((3, passenger))
    }

    "leave a passenger off" in {
      val passenger = Passenger(goingToFloor = 3)
      val origTakingTo =
        Set(
          passenger,
          Passenger(goingToFloor = 5),
          Passenger(goingToFloor = 1)
        )
      val (newTakingTo, delivered) =
        elevator.leavePassengerOff(passenger, origTakingTo, List.empty)

      newTakingTo shouldNot contain(passenger)
      delivered should contain(passenger)
    }
  }

  "An elevator" when {
    val elevator = new Elevator(10)
    "called" should {
      "collect the calling passenger and deliver to requested floor" in {
        val elevator = new Elevator(10)
        elevator.elevatorCall(1, Passenger(goingToFloor = 9))
        elevator.getElevatorState.collectFrom should contain(
          (1, Passenger(goingToFloor = 9))
        )

        elevator.move()
        val elevatorState = elevator.getElevatorState
        elevatorState.collectFrom should be(empty)
        elevatorState.delivered should contain(Passenger(goingToFloor = 9))
      }

      "collect multiple calling passengers and deliver to requested floors" in {
        val elevator = new Elevator(10)
        val passenger1 = Passenger(goingToFloor = 9)
        val passenger2 = Passenger(goingToFloor = 2)
        val passenger3 = Passenger(goingToFloor = 3)
        elevator.elevatorCall(1, passenger1)
        elevator.elevatorCall(7, passenger2)
        elevator.elevatorCall(5, passenger3)
        elevator.getElevatorState.collectFrom should contain((1, passenger1))
        elevator.getElevatorState.collectFrom should contain((7, passenger2))
        elevator.getElevatorState.collectFrom should contain((5, passenger3))

        elevator.move()
        val elevatorState = elevator.getElevatorState
        elevatorState.collectFrom should be(empty)
        elevatorState.delivered should contain(passenger1)
        elevatorState.delivered should contain(passenger2)
        elevatorState.delivered should contain(passenger3)
      }


    }
  }



}

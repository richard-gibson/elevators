package com.elevators

import scala.annotation.tailrec

class Elevator(floors: Int) extends ElevatorBehaviour {
  private var collectingPassengerFrom: Set[(Int, Passenger)] = Set.empty
  private var takingPassengersTo: Set[Passenger] = Set.empty
  private var passengersDelivered: List[Passenger] = List.empty
  private var currentFloor: Int = 0

  def getElevatorState: ElevatorState =
    ElevatorState(
      currentFloor,
      collectingPassengerFrom,
      takingPassengersTo,
      passengersDelivered
    )

  def elevatorCall(floor: Int, passenger: Passenger): Unit =
    collectingPassengerFrom =
      collectPassengerFrom(floor, passenger, collectingPassengerFrom)

  @tailrec
  final def move(): Unit = {
    println(s"elevatorState to $getElevatorState")
    val (collectFrom, takeTo, delivered) =
      genFloorsToVisit(
        currentFloor,
        collectingPassengerFrom,
        takingPassengersTo,
        passengersDelivered
      )

    val nextFloor: Option[Int] =
      (takeTo.headOption map (_.goingToFloor))
        .orElse(collectFrom.headOption map (_._1))
    val nextFloorToVisit = getNextFloor(currentFloor, nextFloor)

    collectingPassengerFrom = collectFrom
    takingPassengersTo = takeTo
    passengersDelivered = delivered

    if (nextFloor.isDefined) {
      currentFloor = nextFloorToVisit
      move()
    } else {
      idle()
    }

  }

  def idle(): Unit =
    println("waiting....")

}

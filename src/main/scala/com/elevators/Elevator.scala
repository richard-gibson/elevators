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

trait ElevatorBehaviour {
  def collectPassengerFrom(
    floorNo: Int,
    passenger: Passenger,
    collectFrom: Set[(Int, Passenger)]
  ): Set[(Int, Passenger)] = {
    println(s"collecting passenger from floor $floorNo")
    collectFrom + ((floorNo, passenger))
  }

  def takePassengerTo(passenger: Passenger,
                      takeTo: Set[Passenger]): Set[Passenger] = {
    println(s"taking passenger to floor $passenger")
    takeTo + passenger
  }

  def leavePassengerOff(
    passenger: Passenger,
    takeTo: Set[Passenger],
    delivered: List[Passenger]
  ): (Set[Passenger], List[Passenger]) = {
    println("leaving passenger off at floor")
    (takeTo - passenger, passenger :: delivered)

  }

  def passengerCollected(
    floorNo: Int,
    passenger: Passenger,
    collectFrom: Set[(Int, Passenger)],
    takeTo: Set[Passenger]
  ): (Set[(Int, Passenger)], Set[Passenger]) = {
    println(s"collected passenger from floor $floorNo")
    val (matchPassengers, passengersToCollect) = collectFrom partition (_._1 == floorNo)
    (passengersToCollect,
     matchPassengers.foldLeft(takeTo)(
       (acc, elem) => takePassengerTo(elem._2, acc)
     ))
  }

  def genFloorsToVisit(
    currentFlr: Int,
    collectFrom: Set[(Int, Passenger)],
    takeTo: Set[Passenger],
    delivered: List[Passenger]
  ): (Set[(Int, Passenger)], Set[Passenger], List[Passenger]) =
    (collectFrom.exists(_._1 == currentFlr),
     takeTo contains Passenger(currentFlr)) match {
      case (false, false) => (collectFrom, takeTo, delivered)
      case (false, true) =>
        val (t, d) =
          leavePassengerOff(Passenger(currentFlr), takeTo, delivered)
        (collectFrom, t, d)
      case (true, false) =>
        val (c, t) = passengerCollected(
          currentFlr,
          Passenger(currentFlr),
          collectFrom,
          takeTo
        )
        (c, t, delivered)
      case (true, true) =>
        val (c, t1) = passengerCollected(
          currentFlr,
          Passenger(currentFlr),
          collectFrom,
          takeTo
        )
        val (t2, d) = leavePassengerOff(Passenger(currentFlr), t1, delivered)
        (c, t2, d)
    }

  def getNextFloor(currentFlr: Int, nextFloor: Option[Int]): Int =
    nextFloor match {
      case Some(n) if n > currentFlr =>
        currentFlr + 1
      case Some(n) if n < currentFlr =>
        currentFlr - 1
      case _ =>
        currentFlr
    }

}

/*
 * Copyright year author
 */
package com.elevators

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case class UntestableElevatorState(floorNo: Int,
                                   collectFrom: mutable.Set[Int],
                                   takeTo: mutable.Set[Int])

class UntestableElevator(floors: Int) {

  private val collectingPassengerFrom: mutable.Set[Int] =
    mutable.LinkedHashSet.empty
  private val takingPassengersTo: mutable.Set[Int] =
    mutable.LinkedHashSet.empty
  private var currentFloor: Int = 0

  def collectPassengerFrom(floor: Int): Unit = {
    println(s"collecting passenger from floor $floor")
    collectingPassengerFrom += floor
  }

  def takePassengerTo(floor: Int): Unit = {
    println(s"taking passenger to floor $floor")
    takingPassengersTo += floor
  }

  def leavePassengerOff(): Unit = {
    println("leaving passenger off at floor")
    takingPassengersTo -= currentFloor
  }

  def passengerCollected(): Unit = {
    val passengersFloor: Int = Random.nextInt(floors + 1)
    println(s"collected passenger from floor $currentFloor")
    collectingPassengerFrom -= currentFloor
    takePassengerTo(passengersFloor)
  }

  def getElevatorState =
    UntestableElevatorState(currentFloor,
                            collectingPassengerFrom,
                            takingPassengersTo)

  @tailrec
  final def move(): Unit = {
    println(s"moved to current floor $currentFloor")

    val nextPassengerFloor =
      collectingPassengerFrom.headOption.orElse(takingPassengersTo.headOption)

    val nextFloor = nextPassengerFloor match {
      case Some(n) if n > currentFloor =>
        currentFloor + 1
      case Some(n) if n < currentFloor =>
        currentFloor - 1
      case Some(_) =>
        currentFloor
      case None =>
        0
    }

    if (collectingPassengerFrom contains currentFloor)
      passengerCollected()

    if (takingPassengersTo contains currentFloor)
      leavePassengerOff()

    currentFloor = nextFloor

    if (nextPassengerFloor.isEmpty)
      idle()
    else
      move()
  }

  def idle(): Unit =
    println("waiting....")

}

object UntestableElevatorApp extends App {
  val elevator = new UntestableElevator(7)
  elevator.move()

  elevator.collectPassengerFrom(3)

  val elevatorState = elevator.getElevatorState
  println("elevator state ")
  elevator.collectPassengerFrom(2)
  elevator.collectPassengerFrom(5)
  elevator.collectPassengerFrom(1)

  elevator.move()

}

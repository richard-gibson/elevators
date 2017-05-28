package com.elevators

import akka.actor.Actor.Receive
import akka.actor.{ Actor, ActorLogging, ActorRef, Props }

/**
  * Created by richardgibson on 28/05/2017.
  */
class ElevatorActor(floors: Int, notificationListener: ActorRef)
    extends Actor
    with ActorLogging
    with ElevatorBehaviour {

  private var collectingPassengerFrom: Set[(Int, Passenger)] = Set.empty
  private var takingPassengersTo: Set[Passenger] = Set.empty
  private var passengersDelivered: List[Passenger] = List.empty
  private var currentFloor: Int = 0
  private var isMoving: Boolean = false

  override def receive: Receive = {
    case ElevatorStateRequest =>
      log.info("Elevator state requested")
      sender ! ElevatorState(
        currentFloor,
        collectingPassengerFrom,
        takingPassengersTo,
        passengersDelivered
      )

    case PassengerToCollect(floor, passenger) =>
      collectingPassengerFrom =
        collectPassengerFrom(floor, passenger, collectingPassengerFrom)
      if (!isMoving) {
        isMoving = true
        self ! Move
        notificationListener ! Moving
      }

    case Move =>
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
        self ! Move
      } else {
        log.info(
          s"${ElevatorState(currentFloor, collectingPassengerFrom, takingPassengersTo, passengersDelivered)}"
        )
        isMoving = false
        notificationListener ! Idle
      }
  }

}

object ElevatorActor {
  def apply(floors: Int, notificationListener: ActorRef): Props =
    Props(new ElevatorActor(floors, notificationListener))
}

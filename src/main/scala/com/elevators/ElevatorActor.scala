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


  override def receive(): Receive = idleReceive(List.empty, 0)


  def movingReceive(collectingPassengerFrom: Set[(Int, Passenger)],
                    takingPassengersTo: Set[Passenger],
                    passengersDelivered: List[Passenger],
                    currentFloor: Int): Receive = {
    case PassengerToCollect(floor, passenger) =>

      context become movingReceive(collectPassengerFrom(floor, passenger, collectingPassengerFrom),
        takingPassengersTo, passengersDelivered, currentFloor)
      self ! Move

    case Move =>
      val (collectFrom, takeTo, delivered) =
        genFloorsToVisit(currentFloor, collectingPassengerFrom, takingPassengersTo, passengersDelivered)

      val nextFloor: Option[Int] = (takeTo.headOption map (_.goingToFloor))
        .orElse(collectFrom.headOption map (_._1))
      val nextFloorToVisit = getNextFloor(currentFloor, nextFloor)


      if (nextFloor.isDefined) {
        context become movingReceive(collectFrom, takeTo, delivered, nextFloorToVisit)
        self ! Move
      } else {
        context become idleReceive(passengersDelivered, currentFloor)
        notificationListener ! Idle
      }

    case ElevatorStateRequest =>
      log.info("Elevator state requested")
      sender ! ElevatorState(
        currentFloor,
        collectingPassengerFrom,
        takingPassengersTo,
        passengersDelivered
      )

  }


  def idleReceive(passengersDelivered: List[Passenger], currentFloor: Int): Receive = {
    case PassengerToCollect(floor, passenger) =>

      context become movingReceive(collectPassengerFrom(floor, passenger, Set.empty),
        Set.empty, passengersDelivered, currentFloor)
      self ! Move


    case ElevatorStateRequest =>
      log.info("Elevator state requested")
      sender ! ElevatorState(
        currentFloor,
        Set.empty,
        Set.empty,
        passengersDelivered
      )

  }
}

object ElevatorActor {
  def apply(floors: Int, notificationListener: ActorRef): Props =
    Props(new ElevatorActor(floors, notificationListener))
}

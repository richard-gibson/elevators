package com.elevators

import akka.pattern.ask
import akka.actor.{ Actor, ActorLogging, ActorRef, Props }

import scala.concurrent.Future

/**
  * Created by richardgibson on 31/05/2017.
  */
class ElevatorControlSystem(noOfElevators: Int, floors: Int) extends Actor with ActorLogging {
  val elevators: List[ActorRef] =
    (1 to noOfElevators) foreach (_ => context.actorOf(ElevatorActor.props(floors, self))).toList


  override def receive: Receive = {

    case Idle => log.info(s"elevator ${sender().path.name} idle")

    case p@PassengerToCollect(floor, passenger) =>
      //find the least busy elevator by requesting

      val leastBusyElevator: Future[ActorRef] =
        Future.sequence(
          elevators.map(
            elevator => getElevatorWorkLoad(elevator)
              .map((_, elevator))
          )).map(_.sortBy(_._1)).map(_.head._2)
      leastBusyElevator.map(_ ! p)

  }


  def getElevatorWorkLoad(elevator: ActorRef): Future[Int] =
    (elevator ? ElevatorStateRequest)
      .mapTo[ElevatorState]
      .map(state => state.collectFrom.size + state.takeTo.size)


  def manageElevators(idleElevators: List[ActorRef]) = {

  }


}

object ElevatorControlSystem {
  def props(elevators: Int, floors: Int): Props = Props(new ElevatorControlSystem(elevators, floors))
}


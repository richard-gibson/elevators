package com.elevators

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

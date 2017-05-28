/*
 * Copyright year author
 */
package com

package object elevators {

  case class Passenger(goingToFloor: Int)
  case class ElevatorState(floorNo: Int,
                           collectFrom: Set[(Int, Passenger)],
                           takeTo: Set[Passenger],
                           delivered: List[Passenger])
}

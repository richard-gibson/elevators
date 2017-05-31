package futures

import scala.concurrent.Future

trait PersonInfo {
  def personById(pin: Int): Future[Person]
  def addressById(id: Int): Future[Address]
  def addressesInTown(town: String): Future[Seq[Address]]
  def peopleByAddressId(addressId: Int): Future[Seq[Person]]
  def livingWithPerson(person: Person): Future[HouseInfo]
  def allHouseInfo: Future[Seq[HouseInfo]]
  def livingInTown(person: Person): Future[Seq[Person]]
}

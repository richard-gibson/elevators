package futures

import scala.concurrent.{ duration, Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import duration._

case class Person(pin: Int,
                  firstName: String,
                  lastName: String,
                  addressId: Int)

case class Address(id: Int,
                   houseNo: Int,
                   street: String,
                   town: String,
                   postcode: String)

case class Relation(parentId: Int, childId: Int)

case class NotFoundException(e: Exception) extends Exception(e)

case class HouseInfo(address: Address, inhabitants: Seq[Person])

class PersonInfoService extends PersonInfo {
  import PersonInfoModel._

  override def personById(pin: Int): Future[Person] =
    Future((people filter (_.pin == pin)).head).recoverWith {
      case e: NoSuchElementException =>
        Future.failed[Person](NotFoundException(e))
    }

  override def addressById(id: Int): Future[Address] =
    Future((addresses filter (_.id == id)).head).recoverWith {
      case e: NoSuchElementException =>
        Future.failed[Address](NotFoundException(e))
    }

  override def addressesInTown(town: String): Future[Seq[Address]] =
    Future(addresses filter (_.town == town))

  override def peopleByAddressId(addressId: Int): Future[Seq[Person]] =
    Future(people filter (_.addressId == addressId))

  override def livingWithPerson(person: Person): Future[HouseInfo] =
    for {
      address <- addressById(person.addressId)
      inhabitants <- peopleByAddressId(address.id)
    } yield HouseInfo(address, inhabitants)

  override def allHouseInfo: Future[Seq[HouseInfo]] =
    Future.sequence(people map livingWithPerson).map(_.distinct)

  override def livingInTown(person: Person): Future[Seq[Person]] =
    for {
      address <- addressById(person.addressId)
      matchingAddresses <- addressesInTown(address.town)
      matchingAddressIds = matchingAddresses map (_.id)
    } yield
      people filter (person => matchingAddressIds contains person.addressId)
}

object PersonInfoService extends App {

  val p = new PersonInfoService
  println(Await.result(p.personById(3), 5 second))

  val houseInfo =
    Await.result(
      p.personById(1).flatMap(person => p.livingWithPerson(person)),
      5 second
    )
  println(houseInfo)

  println(Await.result(p.allHouseInfo, 5 second))
  println(
    Await.result(p.livingInTown(Person(2, "lynda", "smyth", 1)), 5 second)
  )

}

object PersonInfoModel {

  val people = Seq(
    Person(1, "bob", "smyth", 1),
    Person(2, "lynda", "smyth", 1),
    Person(3, "michael", "smyth", 2),
    Person(4, "jane", "smyth", 1),
    Person(5, "ivan", "jones", 3),
    Person(6, "geoff", "jones", 3),
    Person(7, "aaron", "johnston", 4),
    Person(8, "roisin", "hughes", 5),
    Person(9, "gareth", "hughes", 5),
    Person(10, "keith", "hughes", 5),
    Person(11, "barry", "hughes", 5),
    Person(12, "richard", "johnston", 4),
    Person(13, "emma", "johnston", 4),
    Person(14, "ben", "johnston", 4)
  )

  val addresses = Seq(
    Address(1, 145, "park st", "ballymena", "BT29 5JR"),
    Address(2, 20, "kells rd", "ballymena", "BT29 1AJ"),
    Address(3, 1, "river st", "comber", "BT23 5SR"),
    Address(4, 400, "antrim rd", "belfast", "BT15 2AJ"),
    Address(5, 15, "park rd", "belfast", "BT7 3LN")
  )

}

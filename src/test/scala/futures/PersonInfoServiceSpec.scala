package futures

import org.scalatest.{ AsyncWordSpecLike, Matchers }
import Matchers._

class PersonInfoServiceSpec extends AsyncWordSpecLike {

  "The Person Information Service" when {
    val personInfoService: PersonInfo = new PersonInfoService
    "given an id" should {
      "retrieve the correct person when given a pin" in {
        personInfoService.personById(3) map { person =>
          person shouldBe Person(3, "michael", "smyth", 2)
        }
      }

    }

    "given a person" should {
      "retrieve a persons coinhabitants" in {
        personInfoService.livingWithPerson(Person(2, "lynda", "smyth", 1)) map {
          houseInfo =>
            houseInfo shouldBe HouseInfo(
              Address(1, 145, "park st", "ballymena", "BT29 5JR"),
              Seq(
                Person(1, "bob", "smyth", 1),
                Person(2, "lynda", "smyth", 1),
                Person(4, "jane", "smyth", 1)
              )
            )
        }
      }
      "retrieve people living in the same town" in {
        personInfoService.livingInTown(Person(2, "lynda", "smyth", 1)) map {
          peopleLivingInTown =>
            peopleLivingInTown shouldBe Seq(
              Person(1, "bob", "smyth", 1),
              Person(2, "lynda", "smyth", 1),
              Person(3, "michael", "smyth", 2),
              Person(4, "jane", "smyth", 1)
            )
        }

      }
    }

    "instantiated" should {
      "retrieve groupedHouseInfo when requested" in {
        personInfoService.allHouseInfo map { housingInfo =>
          housingInfo shouldBe Seq(
            HouseInfo(
              Address(1, 145, "park st", "ballymena", "BT29 5JR"),
              Seq(
                Person(1, "bob", "smyth", 1),
                Person(2, "lynda", "smyth", 1),
                Person(4, "jane", "smyth", 1)
              )
            ),
            HouseInfo(
              Address(2, 20, "kells rd", "ballymena", "BT29 1AJ"),
              Seq(Person(3, "michael", "smyth", 2))
            ),
            HouseInfo(
              Address(3, 1, "river st", "comber", "BT23 5SR"),
              Seq(
                Person(5, "ivan", "jones", 3),
                Person(6, "geoff", "jones", 3)
              )
            ),
            HouseInfo(
              Address(4, 400, "antrim rd", "belfast", "BT15 2AJ"),
              Seq(
                Person(7, "aaron", "johnston", 4),
                Person(12, "richard", "johnston", 4),
                Person(13, "emma", "johnston", 4),
                Person(14, "ben", "johnston", 4)
              )
            ),
            HouseInfo(
              Address(5, 15, "park rd", "belfast", "BT7 3LN"),
              Seq(
                Person(8, "roisin", "hughes", 5),
                Person(9, "gareth", "hughes", 5),
                Person(10, "keith", "hughes", 5),
                Person(11, "barry", "hughes", 5)
              )
            )
          )
        }
      }

    }
  }
}

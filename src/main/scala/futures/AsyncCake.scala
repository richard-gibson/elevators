package futures

import scala.concurrent.{ duration, Await, Future, Promise }
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

sealed trait EggSize
case object Small extends EggSize
case object Medium extends EggSize
case object Large extends EggSize

case class Egg(size: EggSize)
case class Flour(weight: Int)
case class Butter(weight: Int)
case class Mixture(flour: Flour, butter: Butter, eggs: Seq[Egg])
case class Cake(flour: Flour, butter: Butter, eggs: Seq[Egg])

case class DodgyButterException(message: String) extends Exception(message)

object AsyncCake extends App {

  import CakeService._

  println("------------------")
  println("callback hell  cake")
  val cakePromise = Promise[Cake]()
  val callbackHellCake = cakePromise.future
  retrieveFlour(400) onComplete {
    case Success(flour) =>
      retrieveDodgyButter(200) onComplete {
        case Success(butter) =>
          retrieveEggs(Large, 3) onComplete {
            case Success(eggs) =>
              mixCake(flour, butter, eggs) onComplete {
                case Success(mixture) =>
                  bakeCake(mixture) onComplete {
                    case Success(cake) => cakePromise success cake
                    case Failure(e) => cakePromise failure e
                  }
                case Failure(e) => cakePromise failure e
              }
            case Failure(e) => cakePromise failure e
          }
        case Failure(e) => cakePromise failure e
      }
    case Failure(e) => cakePromise failure e
  }
  println(Await.ready(callbackHellCake, 5 seconds))

  println("------------------")
  println("flatmap cake")

  val flatMapCake: Future[Cake] =
    retrieveFlour(400) flatMap { flour =>
      retrieveButter(200) flatMap { butter =>
        retrieveEggs(Large, 3) flatMap { eggs =>
          mixCake(flour, butter, eggs) flatMap { mixture =>
            bakeCake(mixture)
          }
        }
      }
    }
  println(Await.ready(flatMapCake, 10 seconds))

  println("------------------")
  println("future sequenced cake")

  def futureSequencedCake: Future[Cake] =
    for {
      flour <- retrieveFlour(400)
      butter <- retrieveButter(200)
      eggs <- retrieveEggs(Large, 3)
      mixture <- mixCake(flour, butter, eggs)
      cake <- bakeCake(mixture)
    } yield cake

  println(Await.ready(futureSequencedCake, 10 seconds))

  println("------------------")
  println("parallel cake")
  val futureParCake: Future[Cake] =
    for {
      ((flour, butter), eggs) <- retrieveFlour(400) zip retrieveButter(200) zip retrieveEggs(
        Large,
        3
      )
      mixture <- mixCake(flour, butter, eggs)
      cake <- bakeCake(mixture)
    } yield cake

  println(Await.ready(futureParCake, 10 seconds))

  println("------------------")
  println("resilient cake")

  val futureDodgyCake: Future[Cake] =
    for {
      flour <- retrieveFlour(400)
      butter <- retrieveDodgyButter(200)
      eggs <- retrieveEggs(Large, 3)
      mixture <- mixCake(flour, butter, eggs)
      cake <- bakeCake(mixture)
    } yield cake

  val resilientCake = futureDodgyCake.recoverWith {
    case _: DodgyButterException => futureSequencedCake
  }

  println(Await.ready(resilientCake, 10 seconds))

}

object CakeService {
  def retrieveFlour(weight: Int): Future[Flour] = {
    println("off to get some flour")
    Future {
      Thread.sleep(400)
      println("flour retrieved")
      Flour(weight)
    }
  }

  def retrieveButter(weight: Int): Future[Butter] = {
    println("off to get some butter")
    Future {
      Thread.sleep(300)
      println("butter retrieved")
      Butter(weight)
    }
  }

  def retrieveDodgyButter(weight: Int): Future[Butter] = {
    println("off to get some dodgy butter")
    Future.failed(DodgyButterException("found some dodgy butter"))
  }

  def retrieveEgg(size: EggSize): Future[Egg] = {
    println(s"off to get an $size Egg")
    Future {
      Thread.sleep(100)
      println("eggs retrieved")
      Egg(size)
    }
  }

  def retrieveEggs(size: EggSize, noOfEggs: Int): Future[Seq[Egg]] = {
    val eggsToRetrieve: Seq[Future[Egg]] = (1 to noOfEggs) map (_ =>
                                                                  retrieveEgg(
                                                                    size
                                                                  ))
    Future.sequence(eggsToRetrieve)
  }

  def mixCake(flour: Flour, butter: Butter, eggs: Seq[Egg]): Future[Mixture] = {
    println(s"off to mix a cake")
    Future {
      Thread.sleep(400)
      println("cake mixed")
      Mixture(flour, butter, eggs)
    }
  }

  def bakeCake(mixture: Mixture): Future[Cake] = {
    println(s"off to bake a cake")
    Future {
      val Mixture(flour, butter, eggs) = mixture
      Thread.sleep(400)
      println("cake baked")
      Cake(flour, butter, eggs)
    }
  }
}

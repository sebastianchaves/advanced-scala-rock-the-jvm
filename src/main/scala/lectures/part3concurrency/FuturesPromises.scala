package com.sebastianchaves.courses
package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

object FuturesPromises extends App {

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calculates on ANOTHER thread
  }

  aFuture.value // Option[Try[Int]]
  println("Waiting on the future")

  aFuture.onComplete {
    case Success(value) => println(s"Value is $value")
    case Failure(e) => println(s"I have failed with $e")
  } // SOME thread

  Thread.sleep(3000)

  // mini social network

  case class Profile(id: String, name: String) {
    def poke(another: Profile): Unit = println(s"$name poking ${another.name}")
  }

  object SocialNetwork {
    // Database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0.dummy" -> "Dummy")

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      // fetching from the DB
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")

  mark.onComplete {
    case Success(markProfile) =>
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete {
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    case Failure(e) => e.printStackTrace()
  }
  Thread.sleep(1000)

  // functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall: Future[String] = mark.map(profile => profile.name)
  val marksBestFriend: Future[Profile] = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted: Future[Profile] = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for-comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  Thread.sleep(1000)

  // fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo {
    SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Chebi Banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(500)
      Transaction(user.name, merchantName, amount, status = "SUCCESS")
    }

    def purchase(userName: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction
      // WAIT for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(userName)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Carlos", "Lavarropa", "Fravega", 500000))

  // promises
  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future

  // thread 1 - consumer
  future.onComplete {
    case Success(r) => println("[consumer] i've received" + r)
  }

  // thread 2 - producer
  val producer = new Thread(() => {
    println("[producer] crunching numbres...")
    Thread.sleep(500)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
  * 1. fulfill a future immediatly with a value
    2. inSequence(fa, fb)
    3. first(fa, fb) => new future with the first value of the two futures
    4. last(fa, fb) => new future with the last value
    5. retryUntil(action: () => Future[T], condition: T => Boolean): Future[T]
  * */

  // 1
  def fulfill[T](value: T): Future[T] = Future.successful(value) // use Future.apply if you need to run in a thread

  // 2
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = first.flatMap(_ => second)

  // 3
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)
    promise.future
  }

  // 4
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    def checkAndComplete: Try[A] => Any = (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }

  first(fast, slow).foreach(f => println(s"First: $f"))
  last(fast, slow).foreach(l => println(s"Last: $l"))

  Thread.sleep(1000)

  // 5
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] = {
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }
  }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println(s"Generated $nextValue")
    nextValue
  }

  retryUntil(action, (x: Int) => x < 20).foreach(result => println(s"settled at $result"))
  Thread.sleep(10000)

  // Recap
  // Future[T] is a computation which will finish at some point
  // Futures are immutable, "read-only" object, we can process them when they are done
  // Promises are "writable-once" container or manager over a future

  // Promise pattern
  // one thread, creates an empty promise who knows how to handle the result
  // another thread holds the promise and fulfills or fails the promise
  // with promise.success(r) or promise.failure(e) or promise.complete(try)

  // success, failure or complete triggers the onComplete in the consuming thread (the first)
}

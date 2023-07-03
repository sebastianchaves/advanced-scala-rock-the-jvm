package com.sebastianchaves.courses
package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  /*
    the producer-consumer problem

    thread1       container     thread2
    producer  ->    [ ? ]   ->  consumer

    thread1 and thread2 run in parallel
  */

  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int): Unit = value = newValue

    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProducerConsumer(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting")
      while(container.isEmpty) {
        println("[consumer] actively waiting")
      }
      println("[consumer] i have consumed" + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] i have produced the value " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

//  naiveProducerConsumer()

  def theory(): Unit = {
    // synchronized
    /* Entering a synchronized expression on an object locks the object

      object's monitor = data structure used by the JVM to keep track of which object is locked by which thread

      only AnyRefs can have synchronized blocks

      Tips:
      - make no assumptions about who gets the lock first
      - keep locking to a minimum
      - maintain thread safety at ALL times in parallel applications
    */
    val someObject = "hello"
    someObject.synchronized { // lock the object's monitor
      // code -> any other thread trying to run this will block
    } // release the lock

    // wait() and notify()
    // wait() on an object's monitor suspends the thread indefinitely

    // thread 1
    someObject.synchronized {
      // .. code
      someObject.wait() // release the lock and.. wait
      // then when allowed to proceed, lock the monitor again and continue
      // .. code
    }

    // thread 2
    someObject.synchronized {
      // .. code
      someObject.notify() // signal ONE sleeping thread they may continue
      someObject.notifyAll() // to awaken ALL threads
      // .. code
    } // but only after i'm done and unlock the monitor

  }

  // wait and notify
  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized {
        container.wait()
      }

      // container must have some value
      println("[consumer] i have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] working...")
      Thread.sleep(2000)
      val value = 50
      container.synchronized {
        println("[producer] producing value " + value)
        container.set(value)
        container.notify()
      }
      println("[producer] i have produced the value " + value)
    })

    consumer.start()
    producer.start()
  }

//  smartProdCons()

  /*
    producer -> [ ? ? ? ] -> consumer
  */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity: Int = 9

    def consumer(id: String) = new Thread(() => {
      val name = s"consumer-$id"
      val random = new Random()

      while(true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println(s"[$name] waiting...")
            buffer.wait()
          }

          val x = buffer.dequeue()
          println(s"[$name] i have consumed " + x)

          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    def producer(id: String) = new Thread(() => {
      val name = s"producer-$id"
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println(s"[$name] buffer is full waiting...")
            buffer.wait()
          }

          println(s"[$name] producing " + i)
          buffer.enqueue(i)

          buffer.notify()

          i += 1
        }

        Thread.sleep(random.nextInt(250))
      }
    })

    val consumers = List(consumer("1"), consumer("2"), consumer("3"))
    val producers = List(producer("1"), producer("2"), producer("3"))

    consumers.foreach(_.start())
    producers.foreach(_.start())
  }

//  prodConsLargeBuffer()

  /*
    prod-cons, level 3

    producer 1 -> [ ] -> consumer1
    producer2 ---------- consumer2
  */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer $id] waiting...")
            buffer.wait()
          }

          val x = buffer.dequeue()
          println(s"[consumer $id] i have consumed " + x)

          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[producer $id] buffer is full waiting...")
            buffer.wait()
          }

          println(s"[producer $id] producing " + i)
          buffer.enqueue(i)

          buffer.notify()

          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer = new mutable.Queue[Int]
    val capacity = 20

    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
  }

//  multiProdCons(3, 3)

  /*
  * Exercises:
  1- think of an example where notifyAll acts in a different way than notify?
  2- create a deadlock
  3- create a livelock
  * */

  // 1 notifyAll
  def testNotifyAll() = {
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[thread $i] waiting...")
        bell.wait()
        println(s"[thread $i] lesto...")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println(s"[announcer] dale gas...")

      bell.synchronized {
        bell.notify()
      }
    }).start()
  }

//  testNotifyAll()

  // 2 - deadlock
  case class Friend(name: String) {
    def bow(other: Friend) = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }

    def rise(other: Friend) = {
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }
    }

    def pass(other: Friend) = {
      while (this.side == "right")
        println(s"$this Oh, but please, $other, feel free to pass...")
        switchSide()
        Thread.sleep(1000)
    }

    var side = "right"

    private def switchSide() = {
      if (this.side == "left") side = "right"
      else "left"
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

//  new Thread(() => sam.bow(pierre)).start()
//  new Thread(() => pierre.bow(sam)).start()

  // 3 - livelock
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()


}

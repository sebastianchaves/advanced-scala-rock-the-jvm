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
    val capacity: Int = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] waiting...")
            buffer.wait()
          }

          val x = buffer.dequeue()
          println("[consumer] i have consumed " + x)

          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full waiting...")
            buffer.wait()
          }

          println("[producer] producing " + i)
          buffer.enqueue(i)

          buffer.notify()

          i += 1
        }

        Thread.sleep(random.nextInt(250))
      }
    })

    consumer.start()
    producer.start()
  }

  prodConsLargeBuffer()

}

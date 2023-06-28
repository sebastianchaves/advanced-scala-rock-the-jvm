package com.sebastianchaves.courses
package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App { // principal JVM thread

  /*
    interface Runnable {
      public void run();
    }
  */
  // JVM threads
  val runnable = new Runnable {
    def run(): Unit = println("im running")
  }
  val aThread: Thread = new Thread(new Runnable {
    override def run(): Unit = println("Running in parallel")
  })

  // give the signal to the JVM to start a JVM thread
  aThread.start() // create a new JVM thread => on top of OS thread
  runnable.run() // doesnt do anything in parallel

  aThread.join() // blocks until aThread finishes running
  // that make sure that a thread has already run before you continue some computations

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  // threads run in parallel
  threadHello.start()
  threadGoodbye.start()
  // different runs produce different results!

  // executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 second")
  })

  pool.shutdown()
//  pool.execute(() => println("should not appear")) // throws an exception in the calling thread (main thread = App)

//  pool.shutdownNow()
  println(pool.isShutdown)

}

package loom

import org.junit.Test
import org.junit.Assert._

import java.util._
import scala.concurrent._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits._

class Test1 {

  @Test def checkFiberAwait(): Unit = {
    val promiseToWait = Promise[Int]()
    val future1 = Fiber.spawn{
       println(s"spawned fiber, name=${Thread.currentThread.getName()}")       
       println("waiting for promise")
       val x = Loom.await(promiseToWait.future)
       println("await finished")
       x+1
    }
    val timer = new Timer()
    timer.schedule(new TimerTask() {
       override def run():Unit = {
         println("setting promise")
         promiseToWait success 3
       }
    },1000)
    val r = Await.result(future1, 2 seconds)
    assertEquals(r, 4)
  }


}

package loom

import java.util.concurrent.atomic._
import java.util.concurrent.locks._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._


object Loom {

  // for test
  def runBlockedInFiber(f: =>Unit)(given ec: ExecutionContext):Unit = {
     if (Thread.currentThread.isLightweight()) {
          f 
     } else {
          val fu = Fiber.spawn(f)
          Await.ready(fu, Duration.Inf)
     }
  }

  /**
   * await future, by parking current thread until feature complete.
   * It is effectively
   **/
  def await[T](f:Future[T], atMost: Duration = Duration.Inf)(given ec: ExecutionContext):T = {
     ready(f,atMost).value match {
        case None  =>  throw new TimeoutException() 
        case Some(r) => r match {
           case Success(t) => t
           case Failure(e) => 
                    // TODO: add stack-trace
                    throw e
        }
     }
  }

  def ready[T](f:Future[T], atMost: Duration = Duration.Inf)(given ec: ExecutionContext): Future[T] = {
     if (!f.isCompleted) {
        val awaitDone = new AtomicBoolean(false)
        val nParkLoops = new AtomicInteger(0)
        val waitThread = Thread.currentThread()
        f.onComplete{ _ => if(!awaitDone.get()) {
                             val startNParkLoops = nParkLoops.get()
                             LockSupport.unpark(waitThread) 
                             // for race-condition, when unpark was called before park start [RA1]
                             //  wait here until we will sure, that unpark was processed
                             while(waitThread.isAlive() 
                                   && 
                                   !awaitDone.get()
                                   && 
                                   nParkLoops.get() == startNParkLoops
                                ) {
                                   LockSupport.unpark(waitThread) 
                                   Thread.`yield`()
                             }
                           }
                    }
        try {
          if (atMost == Duration.Inf) {
            while(!f.isCompleted) {
               //[RA] f can be completed here and unpark can be called before park
               // we handle this case by ensuring in inComplete than nParkLoops is updated
               LockSupport.park()
               nParkLoops.incrementAndGet()
            }
          }else{
            var remainingTime = atMost.toNanos
            while(remainingTime > 0 && !f.isCompleted) {
              val startTime = System.nanoTime()
              //[RA]
              LockSupport.parkNanos(remainingTime)
              nParkLoops.incrementAndGet()
              val parkDuration = System.nanoTime() - startTime
              remainingTime = remainingTime - parkDuration
            }
          }
        } finally {
          awaitDone.set(true)
        }
     }
     f
  }


}

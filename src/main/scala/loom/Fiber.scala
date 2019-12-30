package loom

import java.util.concurrent.{Executor => JExecutor}
import scala.concurrent._

trait FiberNaming {
  def next(ec: ExecutionContext):String
}

object DefaultFiberNaming extends FiberNaming {

  val count = new java.util.concurrent.atomic.AtomicLong(0L)

  def next(ec: ExecutionContext):String = {
      s"Fiber-${count.incrementAndGet()}"
  }

}

object Fiber {


  def spawn[T](f: =>T)(given ec: ExecutionContext
                       /*, naming: FiberNaming = DefaultFiberNaming // bug in dotty */
                       ):Future[T] = {
    val scheduler: JExecutor = (r:Runnable) => ec.execute(r)
    val name: String = DefaultFiberNaming.next(ec)
    val characteristics: Int = 0
    val promise = Promise[T]()
    val fiber = Thread.newLightWeightThread(scheduler,
                                            name,
                                            characteristics,
                                            () => promise.success(f)) 
    fiber.setUncaughtExceptionHandler((t,e) => promise.failure(e))
    fiber.start()
    promise.future
  }

}

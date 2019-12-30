package loom

enum ContinuationStep
  case Done extends ContinuationStep
  case Next(thunk: Try[Unit], ContinuationContext => ContinuationStep) extends ContinuationStep
  case Wait[S](s: Future[S], 
               thunk: (Try[S], ContinuationContext) => ContinuationStep
               ) extends ContinuationStep

class ContinuationContext(wrapper: ContinuationWrapper) {

    def suspend(): Unit = wrapper.suspend()

    def await(future: Future[S]): S =
        unwrapTry(awaitTry(future))

    def awaitTry[S](future: Future[S]): Try[S] = 
       var r: Try[S] 
       future.value match 
          case None =>
             wrapper.submit(ContinuationStep.Wait(future, (cc, x)=>{r = x; wrapper.run()}))
             suspend()
          case Some(x)
             r = x
       r

    def awaitCallback[E](callbackHanlder: (E=>Unit) => Unit): E = {
       var r: E 
       callbackHandler(e => { 
           r = e; 
           wrapper.submit(Next(wrapper.run())) 
       })
       wrapper.suspend()
       r
    }

    /**
     * reset, nostalgy edition
     */
    def reset[T](expr: implicit ContinuationContext, ResetContext[T] => T): T = {
      val cn = new ContinuationWrapper( expr, wrapper.runner )
      cn.run()
      resetContext.value
    }

    def shift[S,T]( expr: (S=>T) => T )(implicit ResetContext[T]): T = {
       var cont = { 
          wrapper.run(); s 
       }
       wrapper.submit(Next( resetContext.value = expr(cont) )) 
       suspend()
       awaitCallback( f => expr(f) )
       s
    }

    private def unwrapTry[X](t: Try[X]): X = {
       t match {
          case Success(x) => x
          case Failure(ex) => 
             throw ex
       }
    }

}


class ContinuationWrapper[T](f: (implicit ContinuationContext) => T,
                                 runner: ExecutionContext) extends Continuation {
                                                                      
     val p = Promise[T]()
     private val queue: Queue[ContinuationStep]

     def future() = p.future()

     def resume(): Future[T] = 
        run()
        p.future()
     
     def start(): Future[T] 
        queue.submit(ContinuationStep.Next{ cc =>
          try 
            val r = f(cc)
            p success r
          catch 
            case ex: Exception =>
              p failure ex
        })
        runner.submit{ () => progress }
        p.future

     def progress():Unit = {
       val cc = new ContinuationContext(this)
       var res: Try[Unit] = Success()
       while(!queue.isEmpty()) 
          val c = queue.get()
          c match 
             case Done => //
             case Error(ex) => 
                   // TODO: merge two errors
                   res = Failure(ex)
             case Next(thunk) => 
                   try 
                     next = thunk(res,cc)
                     queue.put(next)
                   catch
                     case ex: Exception => res = Failure(ex)
             case Wait(future,thunk) =>
                     future.value match 
                       case None => future.onComplete{ x=>
                                       queue put Next((cc)=>thunk(x,cc))
                                    }(runner)
                       case Some(value) => 
                                    next = thunk(x,cc)
                                    queue.put(x)
     }
     

}

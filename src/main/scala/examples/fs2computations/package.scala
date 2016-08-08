package examples

import cats.Eval
import fs2.util.Async
import fs2.{Task, async}

/**
  *
  * For blocking run, you can either call unsafeRun, which blocks the current thread, or unsafeRunSync,
  * which runs the task up to the first async boundary and then returns either the result (in the event there is no async boundary)
  * or returns a continuation
  *
  **/
package object fs2computations {

  val nums = Vector.range[Int](1, 10)

  implicit val S =
    fs2.Strategy.fromFixedDaemonPool(4, "cpu-bound-computations")

  val T = implicitly[Async[Task]]

  val s = async.mutable.Semaphore[Task](0).unsafeRun()

  val longs = nums.map(_.toLong.abs)
  val longsRev = longs.reverse

  val task: Task[Unit] = for {
    // N parallel incrementing tasks and N parallel decrementing tasks
    decrements <- Task.start {
                   //T.traverse(longs) { v =>
                   T.parallelTraverse(longs) { v =>
                     s.decrementBy(v)
                       .map(_ => println(s"${Thread.currentThread.getName} decrement:$v"))
                   }
                 }

    increments <- Task.start {
                   //T.traverse(longsRev) { v =>
                   T.parallelTraverse(longsRev) { v =>
                     s.incrementBy(v)
                       .map(_ => println(s"${Thread.currentThread.getName} increment:$v"))
                   }
                 }

    _ <- decrements: Task[Vector[Unit]]
    _ <- increments: Task[Vector[Unit]]
  } yield ()

  task.unsafeRun

  /******************************************************************************************/
  type TF = String => Task[String]

  def relatedTasks: Task[(TF, TF)] =
    for {
      ref1 ← Async.ref[Task, String]
      ref2 ← Async.ref[Task, String]
    } yield {
      val one = (str: String) => {
        println(s"start computation: $str")
        for {
          _ ← (ref1 setPure str)
          _ = Thread.sleep(3000)
          r2 ← ref2.get
        } yield str ++ r2
      }

      val two = (str: String) => {
        println(s"start computations: $str")
        for {
          _ ← (ref2 setPure str)
          _ = Thread.sleep(2000)
          r1 ← ref1.get
        } yield str ++ r1
      }

      (one, two)
    }

  val start = System.currentTimeMillis
  relatedTasks.flatMap {
    case (a, b) => a("a").async race b("b").async
  }.unsafeRun

  /*
    a("a").async.unsafeRunAsync(_.fold(ex => println(ex.getMessage), { r =>
      println(
          s"${Thread.currentThread.getName}: latency: ${(System.currentTimeMillis - start)} $r")
    }))

    b("b").async.unsafeRunAsync(_.fold(ex => println(ex.getMessage), { r =>
      println(
          s"${Thread.currentThread.getName}: latency: ${(System.currentTimeMillis - start)} $r")
    }))
  */
  /*********************************************/
  def fib(n: Int): Task[Int] = {
    if (n < 2) Task now 1
    else
      for {
        a ← fib(n - 1).async
        b ← fib(n - 2).async
      } yield {
        a + b
      }
  }

  def fib2(n: Int): Eval[Int] = {
    if (n < 2) Eval now 1
    else
    for {
      x ← Eval.defer(fib2(n - 1))
      y ← Eval.defer(fib2(n - 2))
    } yield { x + y }
  }

  fib(15).unsafeRun
  fib2(15).value


  def ackermannO(m: Int, n: Int, maxStack: Int = 1 << 9): Eval[Int] = {

    def step(m: Int, n: Int, stack: Int): Eval[Int] =
      if (stack >= maxStack) Eval.defer(ackermannO(m, n))
      else go(m, n, stack + 1)

    def go(m: Int, n: Int, stack: Int): Eval[Int] =
      (m, n) match {
        case (0, _) => Eval.now(n + 1)
        case (m, 0) => step(m - 1, 1, stack)
        case (m, n) => for {
          internalRec <- step(m, n - 1, stack)
          result      <- step(m - 1, internalRec, stack)
        } yield result
      }

    go(m, n, 0)
  }

  def ackermann(m: Int, n: Int): Int = (m, n) match {
    case (0, _) => n + 1
    case (m, 0) => ackermann(m - 1, 1)
    case (m, n) => ackermann(m - 1, ackermann(m, n - 1))
  }

  ackermann(3, 11) //Ok
  ackermann(3, 12) //this call overflows the stack

  ackermannO(3, 12, 1 << 11).value //Ok
}

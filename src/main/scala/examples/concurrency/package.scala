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
package object concurrency {

  val nums = Vector.range[Int](1, 10)

  implicit val S =
    fs2.Strategy.fromFixedDaemonPool(4, "concurrent-executions")

  val T = implicitly[Async[Task]]

  val semaphore = async.mutable.Semaphore[Task](0).unsafeRun()

  val longs = nums.map(_.toLong.abs)
  val longsRev = longs.reverse

  val task: Task[Unit] = for {
  // N parallel incrementing tasks and N parallel decrementing tasks
    decrements <- Task.start {
      T.parallelTraverse(longs) { v =>
        semaphore.decrementBy(v)
          .map(_ => println(s"${Thread.currentThread.getName} decrement:$v"))
      }
    }

    increments <- Task.start {
      T.parallelTraverse(longsRev) { v =>
        semaphore.incrementBy(v)
          .map(_ => println(s"${Thread.currentThread.getName} increment:$v"))
      }
    }

    _ <- decrements: Task[Vector[Unit]]
    _ <- increments: Task[Vector[Unit]]
  } yield ()

  task.unsafeRun

  /******************************************************************************************/
  type TF = String => Task[String]

  def raceBoth(la: Long, lb: Long): Task[(TF, TF)] =
    for {
      refA ← Async.ref[Task, String]
      refB ← Async.ref[Task, String]
    } yield {
      val a = (str: String) => {
        for {
          _ ← (refA setPure str)
          _ = {
            Thread.sleep(la)
            println(s"${Thread.currentThread().getName}: A - $str")
          }
          r2 ← refB.get
        } yield str ++ r2
      }

      val b = (str: String) => {
        for {
          _ ← (refB setPure str)
          _ = {
            Thread.sleep(lb)
            println(s"${Thread.currentThread().getName}: B - $str")
          }
          r1 ← refA.get
        } yield str ++ r1
      }

      (a, b)
    }

  raceBoth(100,200).flatMap {
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

  import annotation.tailrec

  //Makes every iteration async, like scala Future
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

  //defer computations
  def fib2(n: Int): Eval[BigInt] = {
    if (n < 2) Eval now 1
    else
      for {
        x ← Eval.defer(fib2(n - 1))
        y ← Eval.defer(fib2(n - 2))
      } yield {
        x + y
      }
  }

  def fib3(n: Int): BigInt = {
    @tailrec
    def loop(n: Int, start: BigInt, acc: BigInt): BigInt =
    	if(n == 0) acc else loop(n - 1, acc, start + acc)

    loop(n, 0, 1)
  }

  def factorial0(n: Int) = {
    var acc = 1
    (1 to n).foreach { i => acc *= i }
    acc
  }

  def factorial1(n: Int): Int = {
    if (n == 0) 1 else n * factorial1(n - 1)
  }

  def factorial2(n: Int) = {
    @tailrec
    def loop(acc: Int, n: Int): Int = {
      if(n == 0) acc else loop(acc * n, n - 1)
    }
    loop(1, n)
  }


  //operation complexity: O(n^n) (squared)
  def insertionSort[T: Ordering](xs: List[T]): List[T] = {
    def insert[T: Ordering](x: T, xs: List[T]): List[T] = {
        val ord = implicitly[Ordering[T]]
        xs match {
          case Nil => List(x)
          case head :: tail =>
            if (ord.lteq(x, head)) x :: xs
            else  head :: insert(x, tail)
        }
      }

    xs match {
      case Nil => Nil
      case head :: tail => insert(head, insertionSort(tail))
    }
  }

  fib(15).unsafeRun
  fib2(15).value
  fib3(15)

  factorial2(5)

  insertionSort(6 :: 7 :: 9 :: 45 :: 13 :: Nil)


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
          result <- step(m - 1, internalRec, stack)
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

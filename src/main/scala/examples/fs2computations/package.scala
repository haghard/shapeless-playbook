package examples

import fs2.{Task, async}
import fs2.util.Async

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
                       .map(_ =>
                             println(
                                 s"${Thread.currentThread.getName} decrement:$v"))
                   }
                 }

    increments <- Task.start {
                   //T.traverse(longsRev) { v =>
                   T.parallelTraverse(longsRev) { v =>
                     s.incrementBy(v)
                       .map(_ =>
                             println(
                                 s"${Thread.currentThread.getName} increment:$v"))
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
      ref1 <- Async.ref[Task, String]
      ref2 <- Async.ref[Task, String]
    } yield {
      val one = (str: String) => {
        println(s"start computation: $str")
        for {
          _ <- (ref1 setPure str)
          _ = Thread.sleep(3000)
          r2 <- ref2.get
        } yield str ++ r2
      }

      val two = (str: String) => {
        println(s"start computations: $str")
        for {
          _ <- (ref2 setPure str)
          _ = Thread.sleep(2000)
          r1 <- ref1.get
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
}

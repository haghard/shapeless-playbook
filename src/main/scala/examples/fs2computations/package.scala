package examples

import fs2.{Task, async}
import fs2.util.Async


/**
  *
  *
  * For blocking run, you can either call unsafeRun, which blocks the current thread, or unsafeRunSync,
  * which runs the task up to the first async boundary and then returns either the result (in the event there is no async boundary)
  * or returns a continuation
  */
package object fs2computations {

  val nums = Vector.range[Int](1, 50)

  implicit val S = fs2.Strategy.fromFixedDaemonPool(4, "cpu-bound-computations")

  val T = implicitly[Async[Task]]

  val s = async.mutable.Semaphore[Task](0).unsafeRun()

  val longs = nums.map(_.toLong.abs)
  val longsRev = longs.reverse

  val task: Task[Unit] =
    for {
    // N parallel incrementing tasks and N parallel decrementing tasks
      decrs <- Task.start {
        //T.traverse(longs) { v =>
        T.parallelTraverse(longs) { v =>
          s.decrementBy(v).map(_ => println(s"${Thread.currentThread.getName} decrement:$v"))
        }

        /*T.parallelTraverse(longs) { v =>
          s.decrementBy(v).map(_=> println(s"${Thread.currentThread.getName} decrement:$v"))
        }*/
      }

      incrs <- Task.start {
        T.traverse(longsRev) { v =>
          //T.parallelTraverse(longsRev) { v =>
          s.incrementBy(v).map(_ => println(s"${Thread.currentThread.getName} increment:$v"))
        }
      }

      _ <- decrs: Task[Vector[Unit]]
      _ <- incrs: Task[Vector[Unit]]
    } yield ()

  task.unsafeRun

  /******************************************************************************************/

  type TF = String => Task[String]

  def dependentTasks: Task[(TF, TF)] =
    for {
      ref1 <- Async.ref[Task, String]
      ref2 <- Async.ref[Task, String]
    } yield {
      val one = (str: String) => {
        println("start " + str)
        for {
          _ <- (ref1 setPure str)
          _ = Thread.sleep(1000)
          r2 <- ref2.get
        } yield str ++ r2
      }

      val two = (str: String) => {
        println("start " + str)
        for {
          _ <- (ref2 setPure str)
          _ = Thread.sleep(2000)
          r1 <- ref1.get
        } yield str ++ r1
      }

      (one, two)
    }

  val start = System.currentTimeMillis
  dependentTasks.map { case (a, b) =>
    a("a").async.unsafeRunAsync(_.fold(ex => println(ex.getMessage), { line: String =>
      println(s"${Thread.currentThread.getName}: latency: ${(System.currentTimeMillis - start)} $line")
    }))

    b("b").async.unsafeRunAsync(_.fold(ex => println(ex.getMessage), { line: String =>
      println(s"${Thread.currentThread.getName}: latency: ${(System.currentTimeMillis - start)} $line")
    }))
  }.unsafeRun

}
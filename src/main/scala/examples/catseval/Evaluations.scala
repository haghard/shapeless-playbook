package examples.catseval

import java.util.concurrent.atomic.AtomicReference

import cats.Eval

import scala.collection.mutable

object Evaluations {

  sealed trait EvalVar[A] {
    def read: Eval[A]

    def write(value: A): Eval[Unit]

    def transact[B](f: A => (A, B)): Eval[B]

    def compareAndSet(oldVal: A, newVal: A): Eval[Boolean]

    def modify(f: A => A): Eval[Unit] = transact(a => (f(a), ()))
  }

  object EvalVar {
    def apply[A](value: => A): EvalVar[A] = new EvalVar[A] {
      private val state = new AtomicReference(value)

      override def read = Eval.always(state.get)

      override def write(value: A) = Eval.always(state.set(value))

      override def compareAndSet(oldVal: A, newVal: A) =
        Eval.always(state.compareAndSet(oldVal, newVal))

      override def transact[B](f: A => (A, B)): Eval[B] = {
        for {
          a <- read
          (newA, b) = f(a)
          p <- compareAndSet(a, newA)
          th = Thread.currentThread.getName
          r <- if (p) {
                println(s"$th-success")
                Eval.now(b)
              } else {
                println(s"$th-retry")
                transact(f)
              }
        } yield r
      }
    }
  }

  val Var = EvalVar(mutable.Map[String, Int]().withDefaultValue(0))

  def go = {
    (0 until 50).par.foreach { i =>
      Var.modify { m =>
        val th = Thread.currentThread().getName
        m.updated(th, m(th) + 1)
      }.value
    }

    println(Var.read.value)
  }

  def counter(n: Int): Int = if (n <= 0) 0 else counter(n - 1) + 1
  //counter(100000) //sof

  def counter1(n: Int): Eval[Int] =
    if (n <= 0) Eval.now(0) else Eval.defer(counter1(n - 1)).map(_ + 1)

  def counter2(n: Int): Eval[Int] =
    if (n <= 0) Eval.now(0) else Eval.defer(counter2(n - 1).map(_ + 1))

  def fib(n: Int): BigInt = if (n < 2) BigInt(1) else fib(n - 1) + fib(n - 2)

  def fib1(n: Int): Eval[BigInt] =
    if (n < 2) Eval.now(BigInt(1))
    else
      Eval.defer(fib1(n - 1).flatMap { x =>
        fib1(n - 2).map(y => x + y)
      })

  def fib2(n: Int): Eval[BigInt] =
    if (n < 2) Eval.now(BigInt(1))
    else
      for {
        x ← Eval.defer(fib2(n - 1))
        y <- Eval.defer(fib2(n - 2))
      } yield { x + y }
}

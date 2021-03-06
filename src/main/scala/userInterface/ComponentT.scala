package userInterface

import cats.Comonad
import cats.effect.IO
import userInterface.Pairing.Pairing


/**
  * Defined from §4.2.1 of Comonads for UIs starting page 29
  *
  */
object ComponentT {
  // me: there is something a bit weird about these two Units. It feels like it means that
  // the monads can be simplified. Eg:
  // A List[Unit] is just a number
  // A State[Unit] is just a function (from state to state) ie s => ((),s) is just s => s
  type CallBk[Base[_],M[_]] = (Base[M[Unit]] => Base[Unit])
  type UI[Base[_],M[_],A] = CallBk[Base,M] => A
  type ComponentT[Base[_],W[_],M[_],A] = W[UI[Base,M,A]]

  case class Console( text: String, action: String => IO[Unit])
  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)


  def exploreT[W[_],M[_]](
    componentT: ComponentT[IO,W,M,Console]
  )(implicit
    coM: Comonad[W], Pair: Pairing[W,M]
  ): IO[Unit] = {
    import cats.effect.concurrent.Ref
    import cats.implicits._

    type WCompT[W[_],M[_]] = W[UI[IO,M,Console]]
    def send(ref: Ref[IO,WCompT[W,M]], space: WCompT[W,M]): CallBk[IO,M] =
      (baseAction: IO[M[Unit]]) => baseAction.flatMap{ action =>
        val wcomp: WCompT[W,M] = Pairing.move[W, M, UI[IO, M, Console], Unit](space)(action)
        ref.set(wcomp)
      }
    Ref.of[IO, WCompT[W,M]](componentT).flatMap { ref =>
      val step: IO[Unit] = for {
        space <- ref.get
        f: ((IO[M[Unit]] => IO[Unit]) => Console) = coM.extract(space)
        Console(text, act) = f(send(ref, space))
        _ <- putStrlLn(text)
        input <- readLn
        last <- act(input)
      } yield last

      step.foreverM[Unit]
    }
  }


}

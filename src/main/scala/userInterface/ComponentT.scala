package userInterface

import cats.Comonad
import cats.effect.IO
import userInterface.Component.{putStrlLn, readLn}
import userInterface.Pairing.Pairing
import cats.implicits._


/**
  * Defined from §4.2.1 of Comonads for UIs starting page 29
  *
  */
object ComponentT {
  // me: there is something a bit weird about these two Units. It feels like it means that
  // the monads can be simplified. Eg:
  // A List[Unit] is just a number
  // A State[Unit] is just a function (from state to state) ie s => ((),s) is just s => s
  type UI[Base[_],M[_],A] = (Base[M[Unit]] => Base[Unit]) => A
  type ComponentT[Base[_],W[_],M[_],A] = W[UI[Base,M,A]]

  case class Console( text: String, action: String => IO[Unit])

  def exploreT[W[_],M[Unit]](
    componentT: ComponentT[IO,W,M,Console]
  )(implicit
    coM: Comonad[W], Pair: Pairing[W,M]
  ): IO[Unit] = {
    import cats.effect.concurrent.Ref
    import cats.implicits._

    type WCompT[W[_],M[_]] = W[UI[IO,M,Console]]
    def send(ref: Ref[IO,WCompT[W,M]], space: WCompT[W,M]): IO[M[Unit]] => IO[Unit] =
      (baseAction: IO[M[Unit]]) => for {
        action <- baseAction  // <- this extraction of action is new as compared to Component.explore
      } yield {
        val x: WCompT[W,M] = Pairing.move[W, M, UI[IO, M, Console], Unit](space)(action)
        ref.set(x)
      }
    Ref.of[IO, WCompT[W,M]](componentT).flatMap { ref =>
      val step: IO[Unit] = for {
        space <- ref.get
        f: ((IO[M[Unit]] => IO[Unit]) => Console) = coM.extract(space)
        Console(text, action) = f(send(ref, space))
        _ <- putStrlLn(text)
        input <- readLn
        last <- action(input)
      } yield last

      step.foreverM[Unit]
    }
  }


}

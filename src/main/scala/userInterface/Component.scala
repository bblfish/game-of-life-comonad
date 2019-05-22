package userInterface

import cats.Comonad
import cats.effect.IO
import userInterface.Pairing.Pairing

object Component {

  // in haskell UI base m a = (m() -> base ()) -> a    (p23 of Comonads for UIs)
  // this (I think) indicates that actions is a type with one constructor and that it is constructed on the Unit.
  // so should this be translated as
  // type UI[Act[_],Interface] = (Act[Unit] => IO[Unit]) => Interface
  // When Act is the State Monad that means that it is really just a function.
   type UI[Actions, Interface] = (Actions => IO[Unit]) => Interface

  type Component[W[_],Act,Interface] = W[UI[Act,Interface]]  //Act has to pair with W!
  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)


  case class Console( text: String, action: String => IO[Unit])

  def explore[W[_],M[_],Interface,A](
    component: W[UI[M[A], Interface]]
  )(implicit
    Comonad: Comonad[W], Pair: Pairing[W,M]
  ): IO[Unit] = {
    import cats.effect.concurrent.Ref
    import cats.implicits._

    type WComp = W[UI[M[A], Interface]]
    def send(ref: Ref[IO,WComp], space: W[UI[M[A],Interface]]): M[A] => IO[Unit] =
      (action: M[A]) => {
        val x: WComp = Pairing.move[W, M, UI[M[A], Interface], A](space)(action)
        ref.set(x)
      }
    Ref.of[IO, WComp](component).flatMap { ref =>
      val step: IO[Unit] = for {
        space <- ref.get
        f: ((M[A] => IO[Unit]) => Interface) = Comonad.extract(space)
        Console(text, action) = f(send(ref, space))
        _ <- putStrlLn(text)
        input <- readLn
        last <- action(input)
      } yield last

      step.foreverM[Unit]
    }
  }
}

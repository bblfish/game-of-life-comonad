import Pairing.Pairing
import cats.Comonad
import cats.effect.IO

object Component {

  type UI[Actions, Interface] = (Actions => IO[Unit]) => Interface
  type Component[W[_],Act,Interface] = W[UI[Act,Interface]]  //Act has to pair with W!
  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)


  case class Console( text: String, action: String => IO[Unit])

  def explore[W[_],M[_],Interface,A](
    component: W[UI[M[A], Interface]]
  )(implicit
    CoMonad: Comonad[W], Pair: Pairing[W,M]
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
        f: ((M[A] => IO[Unit]) => Interface) = CoMonad.extract(space)
        Console(text, action) = f(send(ref, space))
        _ <- putStrlLn(text)
        input <- readLn
        last <- action(input)
      } yield last
      
      step.foreverM[Unit]
    }
  }
}

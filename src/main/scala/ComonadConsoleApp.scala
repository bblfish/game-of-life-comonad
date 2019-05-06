import Pairing.Pairing
import cats.Comonad
import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Ref
import cats.syntax._
import cats.implicits._

/**
  *  Translation of comonadic version of console app described in §4.2.1 of
  *   "Comonads for UIs".
  *  https://arthurxavierx.github.io/ComonadsForUIs.pdf
  */
object ComonadConsoleApp extends IOApp {

  //hask: type UI action a = (action -> IO()) -> a )
  type UI[Actions, Interface] = (Actions => IO[Unit]) => Interface
  type Component[W[_],Act,Interface] = W[UI[Act,Interface]]  //M has to pair with W!
  type NEList[A] = NonEmptyList[A]


  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)

  implicit val streamComonad = new Comonad[Stream] {
    override def extract[A](x: Stream[A]): A = x.head

    override def coflatten[A](fa: Stream[A]): Stream[Stream[A]] =
      Stream.iterate(fa)( _.tail )

    override def coflatMap[A, B](fa: Stream[A])(f: Stream[A] => B): Stream[B] =
      coflatten(fa).map(f)

    override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = fa.map(f)
  }

  implicit val streamListPair = new Pairing[Stream,NEList] {
    /**
      * the apply method function has been moved to the end as compared
      * to "Comonads for UIs" def §3.1.1
      * Note that both NonEmptyList and stream should be monads and comonads, so this example is perhaps
      * not so good to show the monad/comoand pairing.
      * This could be because (need to check) a list is the initial algebra and stream the final
      * coalgebra and final and initial algebras can be seen in both ways.
      */
    override def apply[A, B, C](fa: Stream[A], gb: NEList[B])(f: (A, B) => C): C = {
      (fa,gb) match {
        case (headS #:: tailS, NonEmptyList(headL,tailL)) =>
          if (tailS.isEmpty || tailL.isEmpty) f(headS,headL)
          else this.apply[A,B,C](tailS,NonEmptyList(tailL.head,tailL.tail))(f)
      }
    }
  }




  def unfoldStream[S,A](state: S)(next: S => (A,S)): Stream[A] = {
    val (a, nextState) = next(state)
    a #:: unfoldStream(nextState)(next)
  }

  case class Console( text: String, action: String => IO[Unit])

  // Component[Stream,List,Interface] = Component[W[_],Act[_],Interface]
  val counterComponent: Stream[UI[NEList[Unit],Console]] = {
    def render(state: Int): UI[NEList[Unit], Console] = {
      (send: NEList[Unit] => IO[Unit]) => Console(
        s"state=$state",
        (input: String) => send(NonEmptyList.fromList(List((),())).get)
      )
    }

    unfoldStream[Int, UI[NEList[Unit], Console]](0) { (state: Int) =>
      (render(state), state + 1)
    }
  }

  def explore[W[_],M[_],Interface](
    component: W[UI[M[Unit], Interface]])(  //why does this have to be a Unit monad?
    implicit
    CoMonad: Comonad[W], Pair: Pairing[W,M]
  ): IO[Unit] = {
    type WComp = W[UI[M[Unit], Interface]]
    def send(ref: Ref[IO,WComp], space: W[UI[M[Unit],Interface]])= (action: M[Unit]) => {
      val x: WComp = Pairing.move[W,M,UI[M[Unit],Interface], Unit](space)(action)
      ref.set(x)  //<- intellij has trouble with this but it seems right.
    }
    Ref.of[IO, WComp](component).flatMap { ref =>
      val step: IO[Unit] = for {
        space <- ref.get
        f: ((M[Unit] => IO[Unit]) => Interface) = CoMonad.extract(space)
        Console(text, action) = f(send(ref, space))
        _ <- putStrlLn(text)
        input <- readLn
        last <- action(input)
      } yield last
      step.foreverM[Unit]
    }
  }

  def run(args: List[String]) = explore(counterComponent).map{_=> ExitCode.Success}
}

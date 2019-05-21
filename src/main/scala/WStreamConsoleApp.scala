import Pairing.Pairing
import cats.Comonad
import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}

/**
  *  Translation of comonadic version of console app described in §4.1.2 of
  *   "Comonads for UIs".
  *  https://arthurxavierx.github.io/ComonadsForUIs.pdf
  */
object WStreamConsoleApp extends IOApp {
  import Component._
  
  //hask: type UI action a = (action -> IO()) -> a )
  type NEList[A] = NonEmptyList[A]



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



  // Component[Stream,NEList,Interface] = Component[W[_],Act[_],Interface]
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


  def run(args: List[String]) = explore(counterComponent).map{_=> ExitCode.Success}
}

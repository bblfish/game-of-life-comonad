package userInterface

import cats.effect.{ExitCode, IO, IOApp}

/**
  * Translation of comonadic version console app using the Store comonad
  * and the State monad as described in §4.1.3 of "Comonads for UIs".
  * https://arthurxavierx.github.io/ComonadsForUIs.pdf
  */

object WStoreStateApp extends IOApp {

  import Component._
  import cats.data.{State, Store}
  import cats.implicits._
  import Pairing.storeStatePair

  val listComponent: Component[Store[List[String],?],State[List[String],Unit],Console] = {
    def render(history: List[String]): UI[State[List[String],Unit],Console] =
      (send: State[List[String], Unit] => IO[Unit]) => {
        Console(
          s"I've received $history",
          (input: String) => send(
            if (input == "") {
              State.set(List[String]())
            } else {
              State.modify[List[String]](input :: _)
            })
        )
      }
    Store(render _, List[String]())
  }

  override def run(args: List[String]): IO[ExitCode] =
    explore(listComponent).map{_=> ExitCode.Success}
}

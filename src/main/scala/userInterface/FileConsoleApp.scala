package userInterface

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import cats.data.{State, Store}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import userInterface.ComponentT.{Console, UI, exploreT}

import scala.util.Try

/**
  * Defined from §4.2.1 of Comonads for UIs starting page 29
  */
object FileConsoleApp extends IOApp {
  // type CallBk[Base[_],M[_]] = (Base[M[Unit]] => Base[Unit])
  // type UI[Base[_],M[_],A] = CallBk[Base,M] => A
  // type ComponentT[Base[_],W[_],M[_],A] = W[UI[Base,M,A]]
  //Store[List[String], UI[IO, State[List[String], _], Console]]
  //ComponentT[IO, Store[List[String],_],State[List[String],_],Console]
  import java.nio.file.{FileSystems, Path}

  lazy val pwd: Path = FileSystems.getDefault.getPath(".")

  lazy val filesComponent: Store[List[String], UI[IO, State[List[String], ?], Console]] =
    Store(render _ ,List[String]())

  def render(list: List[String]): (IO[State[List[String], Unit]] => IO[Unit]) => Console =
    (send: IO[State[List[String],Unit]] => IO[Unit]) => {
      Console(s"Files read: $list\n$pwd>",
        (tryReadFile _) andThen send
      )
    }
  

  def tryReadFile(input: String): IO[State[List[String], Unit]] = {
    val read: IO[State[List[String], Unit]] = if (input == null || input == "")
          IO.raiseError(new Error("empty file name"))
        else for {
          content <- inputStream(new File(input)).use { (fin: FileInputStream) =>
            IO.fromTry(Try(new BufferedReader(new InputStreamReader(fin)).readLine()))
          }
          _ <- IO(println(content))
        } yield State.modify[List[String]](input::_)

    read.handleErrorWith(e => IO(println(e)).flatMap(_=>reset))
  }

  val reset: IO[State[List[String], Unit]] =
    IO(println("resetting")).flatMap{_ => IO(State.set[List[String]](List()))}

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO.fromTry(Try(new FileInputStream(f)))
    } { inStream =>
      IO(inStream.close()).handleErrorWith{e => IO(println(e))} // release
    }

  import Pairing.storeStatePair
  import cats.implicits._

  override def run(args: List[String]): IO[ExitCode] = {
    exploreT(filesComponent).map{_=> ExitCode.Success}
  }
}

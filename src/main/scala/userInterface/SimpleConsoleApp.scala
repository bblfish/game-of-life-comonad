package userInterface

import cats.effect.{ExitCode, IO, IOApp}

/**
  *  Translation of simple console app using IO monad from example 4.1.1 in
  * "Comonads for UIs".
  *  https://arthurxavierx.github.io/ComonadsForUIs.pdf
  */
object SimpleConsoleApp extends IOApp {

  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)


  def readNum(n: Int): IO[Int] = {
    for {
      _ <- putStrlLn("Enter a positive integer")
      answer <- readLn
      ai <- IO(Integer.parseInt(answer))
      count <- if (ai<0)
            IO.pure(n)
           else readNum(ai+n)
    } yield count
  }

  def run(args: List[String]) = {
    for {
      _ <- putStrlLn("What's your name?")
      n <- readLn
      _ <- putStrlLn(s"Hello, $n!")
      num <- readNum(0)
      _ <- putStrlLn("total="+num)
    } yield ExitCode.Success
  }

}

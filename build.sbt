name := "game-of-life-comonad"

scalaVersion := "2.13.0-RC2"

scalacOptions ++= Seq(
//  "-Ypartial-unification",  has that been removed? why?
  "-Ywarn-value-discard",
  "-feature",
  "-language:higherKinds" ,
  "-deprecation"
)

mainClass in (Compile, run) := Some("LifeZipper")

libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % "2.0.0-M2",
        "org.typelevel" %% "cats-effect" % "2.0.0-M2",
        "org.scalatest" %% "scalatest" % "3.1.0-SNAP11" % "test"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.1")

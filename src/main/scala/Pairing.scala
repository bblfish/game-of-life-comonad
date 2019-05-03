// translation of Pairing lib 
// https://github.com/paf31/purescript-pairing/blob/v5.1.0/src/Data/Functor/Pairing.purs#L41
// by Luis Martinez @luis3m on Gitter
// https://scastie.scala-lang.org/ZFZ0P6jaS6GVHwcJ3uF1Sg

object Main extends App {
  
  trait Pairing[F[_], G[_]]{
    def apply[A, B, C](f: (A, B) => C, fa: F[A], gb: G[B]): C
  }
  
  type Identity[A] = A

  type <->[F[_], G[_]] = Pairing[F, G]
  
  def sym[F[_], G[_]](pairing: F <-> G): G <-> F = new (G <-> F) {
    def apply[A, B, C](f: (A, B) => C, ga: G[A], fb: F[B]): C =
      pairing[B, A, C]({ case (a, b) => f(b, a) }, fb, ga)
  }
  
  def zap[F[_], G[_], A, B](pairing: F <-> G, fab: F[A => B], ga: G[A]): B =
    pairing[A => B, A, B]({ case (a, b) => a(b) }, fab, ga)
  
  def identity: Identity <-> Identity = new (Identity <-> Identity) {
    def apply[A, B, C](f: (A, B) => C, a: Identity[A], b: Identity[B]): C =
        f(a, b)
  }
  
    println("Hello")
  
}

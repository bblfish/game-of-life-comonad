import cats.Comonad
import cats.data.State

/**
  *
  * How bblfish came to this code.
  *
  * 1. I wanted to understand comonads better programmatically, since they
  *   turn up in the security logic of A says S by Abadi et al.
  *   see "What logic corresponds via Curry Howard to a Monad".
  *   https://cstheory.stackexchange.com/questions/42689/what-logic-correponds-via-curry-howard-to-a-monad/42696
  *   The idea was to try and get as good a practical development experience with them
  *   as possible.
  *
  * 2. The Game of life article "Life is a Comonad" which the code in this repository came
  * from was really very helpful
  *    https://eli-jordan.github.io/2018/02/16/life-is-a-comonad/
  *
  * 3. Along the way I discovered the article "Comonads as Spaces" by
  * Phil Freeman
  *   https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html
  * which argues that by looking at the monad/comonad pairings one can develop a
  * framework for integrating all types of User Interfaces. This is explained
  * in a number of other blog posts and in a short article by Phil
  * "Declarative uis are the future—and the future is comonadic"
  *  https://functorial.com/the-future-is-comonadic/main.pdf
  *  It was also deceloped in a Master's Thesis by A Xavier
  *  and this article:
  *  "A Real-World Application with a Comonadic User Interface"
  *  https://arthurxavierx.github.io/RealWorldAppComonadicUI.pdf
  *  Phil Freeman writes about Pairing of Functors, and how this
  *  lead to the Co-class which allows one to automatically generate
  *  a monad from a comonad.
  *
  *  I was also pointed to this blog post from 2014 "Cofree meets Free"
  *  http://blog.sigfpe.com/2014/05/cofree-meets-free.html
  *
  *  4. Something that goes in the same direction, but looking at it from
  *  the point of view of protocols is "Cofun with Cofree Comonads"
  *     http://dlaing.org/cofun/
  *
  * 5. Another very interesting Haskell older talk on the Streams comonad is
  * "Kenneth Foner - Getting a Quick Fix on Comonads"
  * And it's relation to spreadsheets.
  * https://www.youtube.com/watch?v=F7F-BzOB670
  *
  */

// translation of Pairing lib
// https://github.com/paf31/purescript-pairing/blob/v5.1.0/src/Data/Functor/Pairing.purs#L41
//first published by Luis Martinez by  luis3m@github.com in
//https://scastie.scala-lang.org/ZFZ0P6jaS6GVHwcJ3uF1Sg

object Pairing {

  trait Pairing[F[_], G[_]]{
    /**
     * the apply method function has been moved to the end as compared
     * to "Comonads for UIs" def §3.1.1
     */
    def apply[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
  }

  type Identity[A] = A

  type ⋈[F[_], G[_]] = Pairing[F, G]

  def sym[F[_], G[_]](implicit pairing: F ⋈ G): G ⋈ F = new (G ⋈ F) {
    def apply[A, B, C](ga: G[A], fb: F[B])(f: (A, B) => C): C =
      pairing[B, A, C](fb, ga){ case (a, b) => f(b, a) }
  }

  def zap[F[_], G[_], A, B](pairing: F ⋈ G, fab: F[A => B], ga: G[A]): B =
    pairing[A => B, A, B](fab, ga){ case (a, b) => a(b) }

  def identity: Identity ⋈ Identity = new (Identity ⋈ Identity) {
    def apply[A, B, C](a: Identity[A], b: Identity[B])(f: (A, B) => C): C =
      f(a, b)
  }

  /**
   * Function defined in §3.2.1 of "Comonads as Spaces"
    *
   */
  def move[W[_],M[_],A,B](space: W[A])(movement: M[B])
    (implicit
      W: Comonad[W],
      Pair: Pairing[W,M]
    ): W[A] = // note: I have reversed the position of W and M in the Pairing below...
      Pair(W.coflatten[A](space),movement){case (wa,_) => wa }


  def stateStorePair[S]: State[S, ?] ⋈ Store[S, ?] =
    new (State[S,?] ⋈ Store[S,?]) {
    override def apply[A, B, C](state: State[S, A], store: Store[S,B])(f: (A, B) => C): C = {
      val (s, a) = state.run(store.index).value
      val b = store.lookup(store.index)
      //should we instead use the new state? like this:
      //val b = store.lookup(s)
      f(a, b)
    }
  }
//  def stateStore[F[_],G[_],S](
//    pairing: F ⋈ G
//  ): StateT[F,S,_] ⋈ RepresentableStore[G,S,_] =
//    new Pairing[StateT[F,S,_],RepresentableStore[G,S,_]] {
//
//    }


}
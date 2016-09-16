package cat.s


class StackManip extends ynfrastructure.Spec {

  "Manipulate stack with State[Stack,A]" in {
    //based on this story: http://eed3si9n.com/herding-cats/State.html

    //given Stack
    trait Stack[A] {
      def pop: Stack[A]
      def push(a: A): Stack[A]
      def topItem: A
    }

    //...and the Stack's simple implementation
    case class SimpleStack[A](store: List[A]) extends Stack[A] {
      override def pop = if(store.isEmpty) sys.error("Empty stack") else SimpleStack(store.tail)
      override def push(a: A) = SimpleStack(a :: store)
      override lazy val topItem = if(store.isEmpty) sys.error("Empty stack") else store.head
    }

    import cats.data.State

    def pop[A]: State[Stack[A], A] = State(stack => (stack.pop, stack.topItem))
    def push[A](a: A): State[Stack[A], Unit] = State.modify(_.push(a))

    def stackManip: State[Stack[String], String] = for {
      _ <- push("Stack")
      _ <- push("This")
      _ <- push("dropped")
      _ <- push("Jaw")
      a1 <- pop
      a2 <- pop
    } yield s"$a1 $a2?"

    val initialStack = SimpleStack(List("is", "crazy!"))

    val stackAfterManipulation = SimpleStack(List("This", "Stack", "is", "crazy!"))

    stackManip.run(initialStack).value mustBe (stackAfterManipulation, "Jaw dropped?")
  }
}

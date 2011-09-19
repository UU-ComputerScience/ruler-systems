package stepwise;

public abstract class Node<X, I, E> extends CoroutineBase<I, E> {
	public Node() {
		super(true);
	}

	abstract protected X syns();

	public Stepwise<X, I, E> begin() {
		return new Parents<X, I, E>(this, syns());
	}
}

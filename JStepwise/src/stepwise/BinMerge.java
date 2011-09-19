package stepwise;

public abstract class BinMerge<X,I,E> extends Merge<X, I, E> {
	protected Stepwise<X,I,E> _left;
	protected Stepwise<X,I,E> _right;
	
	public BinMerge(final Stepwise<X,I,E> left, final Stepwise<X,I,E> right) {
		_left = left;
		_right = right;
	}
}

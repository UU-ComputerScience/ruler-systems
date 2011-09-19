package stepwise;

public class ReportChild<I,E> implements Report<I,E> {
	private Stepwise<?,I,E> _child;
	
	public ReportChild(final Stepwise<?,I,E> child) {
		_child = child;
	}
	
	public Stepwise<?,I,E> get() {
		return _child;
	}
}
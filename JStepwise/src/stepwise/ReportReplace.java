package stepwise;

public class ReportReplace<I,E> implements Report<I,E> {
	private Stepwise<?,I,E> _comp;
	
	public ReportReplace(final Stepwise<?,I,E> comp) {
		_comp = comp;
	}
	
	public Stepwise<?,I,E> get() {
		return _comp;
	}
}
package stepwise;

public class ReportFail<I,E> implements Report<I,E> {
	private E _failure;
	
	public ReportFail(final E failure) {
		_failure = failure;
	}
	
	public E get() {
		return _failure;
	}
}
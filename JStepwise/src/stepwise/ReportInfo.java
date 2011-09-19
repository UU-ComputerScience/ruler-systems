package stepwise;

public class ReportInfo<I,E> implements Report<I,E> {
	private I _info;
	
	public ReportInfo(final I info) {
		_info = info;
	}
	
	public I get() {
		return _info;
	}
}
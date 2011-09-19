package stepwise;

public class BacktrackException extends RuntimeException {
	private static final long serialVersionUID = 7278203135987033956L;

	public BacktrackException(final String _reason) {
		super(_reason);
	}
}

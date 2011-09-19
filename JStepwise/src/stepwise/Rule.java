package stepwise;

public abstract class Rule implements Runnable {
	private boolean _hasRun;

	public Rule() {
		_hasRun = false;
	}

	@Override
	public void run() {
		if (!_hasRun) {
			_hasRun = true;
			execute();
		}
	}

	protected abstract void execute();
}

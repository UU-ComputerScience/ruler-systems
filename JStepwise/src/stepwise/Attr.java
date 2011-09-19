package stepwise;

public final class Attr<T> {
	private T _value;
	private Runnable _rule;

	public Attr() {
		_value = null;
		_rule = null;
	}

	public T get() {
		if (_value == null) {
			if (_rule == null) {
				throw new IllegalArgumentException(
						"attribute is not defined by a rule");
			}
			_rule.run();
		}

		return _value;
	}

	public void set(final T value) {
		_value = value;
	}

	public void dependsOn(final Runnable rule) {
		_rule = rule;
	}
}

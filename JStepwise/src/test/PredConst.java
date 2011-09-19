package test;

import stepwise.Rule;

public final class PredConst extends Pred {
	// symbols
	final private boolean _value;

	private int _state;

	final private Rule _rule1;

	public PredConst(final boolean value) {
		_state = 0;

		_value = value;

		// construct rules
		_rule1 = new Rule() {
			public void execute() {
				syns().value().set(_value);
			}
		};

		// setup dependencies of synthesized and local attributes
		syns().value().dependsOn(_rule1);
	}

	@Override
	protected void visit() {
		switch (_state) {
		case 0:
			_state = 1;

			_rule1.run(); // compute syn attr
			emit(new InfoWork());
			done();
			break;
		default:
			done();
			break;
		}
	}
}

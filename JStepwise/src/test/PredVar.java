package test;

import stepwise.Rule;

public final class PredVar extends Pred {
	// symbols
	private final String _name;

	private int _state;

	private final Rule _rule1;

	public PredVar(final String name) {
		_state = 0;

		_name = name;

		// construct rules
		_rule1 = new Rule() {
			public void execute() {
				boolean b = inhs().env().get().get(_name);
				syns().value().set(b);
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

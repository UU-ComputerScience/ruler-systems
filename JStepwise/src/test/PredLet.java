package test;

import java.util.HashMap;

import stepwise.Attr;
import stepwise.BacktrackException;
import stepwise.Rule;
import stepwise.Stepwise;

public final class PredLet extends Pred {
	// symbols
	final private String _name;
	final private Pred _expr;
	final private Pred _body;

	// local attributes
	private final Attr<Pred> _eIn;
	private final Attr<Pred> _bIn;
	private final Attr<Stepwise<PredSyn, Info, BacktrackException>> _eOut;
	private final Attr<Stepwise<PredSyn, Info, BacktrackException>> _bOut;

	private int _state;

	private final Rule _rule1;
	private final Rule _rule2;
	private final Rule _rule3;
	private final Rule _rule4;
	private final Rule _rule5;
	private final Rule _rule6;
	private final Rule _rule7;

	@SuppressWarnings("unchecked")
	public PredLet(final String name, final Pred expr, final Pred body) {
		_state = 0;

		_name = name;
		_expr = expr;
		_body = body;

		_eIn = new Attr<Pred>();
		_bIn = new Attr<Pred>();
		_eOut = new Attr<Stepwise<PredSyn, Info, BacktrackException>>();
		_bOut = new Attr<Stepwise<PredSyn, Info, BacktrackException>>();

		// construct rules
		_rule1 = new Rule() {
			public void execute() {
				HashMap<String, Boolean> env = inhs().env().get();
				_eIn.get().inhs().env().set(env);
			}
		};

		_rule2 = new Rule() {
			public void execute() {
				boolean val = _eOut.get().lazyEval().value().get();
				HashMap<String, Boolean> env = (HashMap<String, Boolean>) inhs()
						.env().get().clone();
				env.put(_name, val);
				_bIn.get().inhs().env().set(env);
			}
		};

		_rule3 = new Rule() {
			public void execute() {
				boolean val = _eOut.get().lazyEval().value().get();
				syns().value().set(val);
			}
		};

		_rule4 = new Rule() {
			public void execute() {
				_eIn.set(_expr);
				_eIn.get().inhs().env().dependsOn(_rule1);
			}
		};

		_rule5 = new Rule() {
			public void execute() {
				_bIn.set(_body);
				_bIn.get().inhs().env().dependsOn(_rule2);
			}
		};

		_rule6 = new Rule() {
			public void execute() {
				_eOut.set(_eIn.get().begin());
			}
		};

		_rule7 = new Rule() {
			public void execute() {
				_bOut.set(_bIn.get().begin());
			}
		};

		// setup dependencies of synthesized and local attributes
		syns().value().dependsOn(_rule3);
		_eIn.dependsOn(_rule4);
		_bIn.dependsOn(_rule5);
		_eOut.dependsOn(_rule6);
		_bOut.dependsOn(_rule7);
	}

	@Override
	protected void visit() {
		switch (_state) {
		case 0:
			_state = 1;

			_rule4.run(); // create expr child
			_rule1.run(); // assign its inh attr
			_rule6.run(); // prepare it
			resumeAfter(_eOut.get());
			break;
		case 1:
			_state = 2;

			_rule5.run(); // create body child
			_rule2.run(); // assign its inh attr
			_rule7.run(); // prepare it
			resumeAfter(_bOut.get());
			break;
		case 2:
			_state = 3;
			_rule3.run(); // assign syn attr
			done();
		default:
			done();
			break;
		}
	}
}

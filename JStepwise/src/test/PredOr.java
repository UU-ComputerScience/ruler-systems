package test;

import stepwise.Attr;
import stepwise.BacktrackException;
import stepwise.Rule;
import stepwise.Stepwise;

public final class PredOr extends Pred {
	// symbols
	private final Pred _left;
	private final Pred _right;

	// local attributes
	private final Attr<Pred> _leftIn;
	private final Attr<Pred> _rightIn;
	private final Attr<Stepwise<PredSyn, Info, BacktrackException>> _resOut;

	private int _state;

	private final Rule _rule1;
	private final Rule _rule2;
	private final Rule _rule3;
	private final Rule _rule4;
	private final Rule _rule5;
	private final Rule _rule6;

	public PredOr(final Pred left, final Pred right) {
		_state = 0;

		_left = left;
		_right = right;

		_leftIn = new Attr<Pred>();
		_rightIn = new Attr<Pred>();
		_resOut = new Attr<Stepwise<PredSyn, Info, BacktrackException>>();

		// construct rules
		_rule1 = new Rule() {
			@Override
			public void execute() {
				_leftIn.get().inhs().env().set(inhs().env().get());
			}
		};

		_rule2 = new Rule() {
			@Override
			public void execute() {
				_rightIn.get().inhs().env().set(inhs().env().get());
			}
		};

		_rule3 = new Rule() {
			@Override
			public void execute() {
				boolean b = _resOut.get().lazyEval().value().get();
				syns().value().set(b);
			}
		};

		_rule4 = new Rule() {
			@Override
			public void execute() {
				_leftIn.set(_left);
				_leftIn.get().inhs().env().dependsOn(_rule1);
			}
		};

		_rule5 = new Rule() {
			@Override
			public void execute() {
				_rightIn.set(_right);
				_rightIn.get().inhs().env().dependsOn(_rule2);
			}
		};

		_rule6 = new Rule() {
			@Override
			public void execute() {
				ChooseOr choice = new ChooseOr(_leftIn.get().begin(), _rightIn
						.get().begin());
				_resOut.set(choice);
			}
		};

		// setup dependencies of synthesized and local attributes
		syns().value().dependsOn(_rule3);
		_leftIn.dependsOn(_rule4);
		_rightIn.dependsOn(_rule5);
		_resOut.dependsOn(_rule6);
	}

	@Override
	protected void visit() {
		switch (_state) {
		case 0:
			_state = 1;

			_rule4.run(); // create left child
			_rule5.run(); // create right child
			_rule1.run(); // assign left's inh attr
			_rule2.run(); // assign right's inh attr
			_rule6.run(); // prepare it
			resumeAfter(_resOut.get());
			break;
		case 1:
			_state = 3;
			_rule3.run(); // assign syn attr
			done();
		default:
			done();
			break;
		}
	}
}

package stepwise;

import java.util.LinkedList;

public abstract class CoroutineBase<I, E> implements Coroutine<I, E> {
	private LinkedList<Report<I, E>> _actions;
	private boolean _mustYieldAction;

	public CoroutineBase(final boolean mustYieldAction) {
		_mustYieldAction = mustYieldAction;
		_actions = new LinkedList<Report<I, E>>();
	}

	@Override
	public Report<I, E> nextStep() {
		Report<I, E> rep = null;
		boolean didVisit = false;

		while (true) {
			rep = _actions.poll();
			if (rep == null) {
				if (didVisit && _mustYieldAction)
					throw new RuntimeException(
							"Visit method did not end with at least one action.");

				visit();
				didVisit = true;
			} else {
				return rep;
			}
		}
	}

	protected abstract void visit();

	protected void emit(final I info) {
		_actions.add(new ReportInfo<I, E>(info));
	}

	protected void resumeAfter(final Stepwise<?, I, E> child) {
		_actions.add(new ReportChild<I, E>(child));
	}

	protected void abort(final E failure) {
		_actions.add(new ReportFail<I, E>(failure));
	}

	protected void done() {
		_actions.add(new ReportDone<I, E>());
	}

	protected void commit(final Stepwise<?, I, E> comp) {
		_actions.add(new ReportReplace<I, E>(comp));
	}
}

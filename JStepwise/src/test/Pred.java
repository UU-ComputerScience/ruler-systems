package test;

import stepwise.BacktrackException;
import stepwise.Node;

public abstract class Pred extends Node<PredSyn, Info, BacktrackException> {
	private PredInh _inhs;
	private PredSyn _syns;

	public Pred() {
		_inhs = new PredInh();
		_syns = new PredSyn();
	}

	@Override
	protected PredSyn syns() {
		return _syns;
	}

	public PredInh inhs() {
		return _inhs;
	}
}

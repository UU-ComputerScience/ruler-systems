package test;

import stepwise.Attr;


public class PredSyn {
	private Attr<Boolean> _value;
	
	public PredSyn() {
		_value = new Attr<Boolean>();
	}
	
	public Attr<Boolean> value() {
		return _value;
	}
}

package test;

import java.util.HashMap;
import stepwise.Attr;

public class PredInh {
	private Attr<HashMap<String,Boolean>> _env;
	
	public PredInh() {
		_env = new Attr<HashMap<String,Boolean>>();
	}
	
	public Attr<HashMap<String,Boolean>> env() {
		return _env;
	}
}

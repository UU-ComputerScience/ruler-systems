package test;

import java.util.HashMap;

import stepwise.BacktrackException;
import stepwise.Stepwise;

public final class Main {
	public static void main(String[] args) {
		// build tree
		Pred p1 = new PredVar("x");
		Pred p2 = new PredVar("y");
		Pred p3 = new PredOr(p1, p2);

		// set inherited attributes of the root
		p3.inhs().env().set(new HashMap<String, Boolean>());
		p3.inhs().env().get().put("x", false);
		p3.inhs().env().get().put("y", true);

		// start on-demand evaluation
		Stepwise<PredSyn, Info, BacktrackException> outcome = p3.begin();
		boolean result = outcome.lazyEval().value().get();
		System.out.println("result: " + result);
	}
}

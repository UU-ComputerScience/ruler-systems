package test;

import stepwise.BacktrackException;
import stepwise.BinMerge;
import stepwise.Report;
import stepwise.ReportDone;
import stepwise.ReportFail;
import stepwise.ReportInfo;
import stepwise.Stepwise;

public class ChooseOr extends BinMerge<PredSyn, Info, BacktrackException> {
	public ChooseOr(Stepwise<PredSyn, Info, BacktrackException> left,
			Stepwise<PredSyn, Info, BacktrackException> right) {
		super(left, right);
	}

	@Override
	protected void visit() {
		Report<Info, BacktrackException> r1 = _left.nextStep();
		Report<Info, BacktrackException> r2 = _right.nextStep();

		if (r1 instanceof ReportDone) {
			System.out.println("ChooseOr.visit(): r1 done");
			commit(_left.lazyEval().value().get() ? _left : _right);
		} else if (r2 instanceof ReportDone) {
			System.out.println("ChooseOr.visit(): r2 done");
			commit(_right.lazyEval().value().get() ? _right : _left);
		} else if (r1 instanceof ReportFail) {
			System.out.println("ChooseOr.visit(): r1 failed");
			commit(_right);
		} else if (r2 instanceof ReportFail) {
			System.out.println("ChooseOr.visit(): r2 failed");
			commit(_left);
		} else if (r1 instanceof ReportInfo && r2 instanceof ReportInfo) {
			System.out.println("ChooseOr.visit(): both stepped");
			emit(new InfoWork());
		} else {
			throw new RuntimeException(
					"ChooseOr.visit(): unexpected report type.");
		}
	}
}

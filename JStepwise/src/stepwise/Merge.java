package stepwise;

public abstract class Merge<X, I, E> extends CoroutineBase<I, E> implements
		Stepwise<X, I, E> {
	public Merge() {
		super(false);
	}

	@Override
	public X lazyEval() {
		while (true) {
			Report<I, E> rep = nextStep();

			if (rep instanceof ReportReplace) {
				ReportReplace<I, E> repl = (ReportReplace<I, E>) rep;
				@SuppressWarnings("unchecked")
				Stepwise<X, I, E> comp = (Stepwise<X, I, E>) repl.get();
				return comp.lazyEval();
			} else if (rep instanceof ReportFail) {
				throw new RuntimeException(
						"Merge.lazyEval(): all alternatives fail.");
			} else if (rep instanceof ReportChild) {
				throw new RuntimeException(
						"Merge.lazyEval(): unexpected ReportChild.");
			} else if (rep instanceof ReportDone) {
				throw new RuntimeException(
						"Merge.lazyEval(): unexpected ReportDone.");
			}

			// else: skip
		}
	}
}

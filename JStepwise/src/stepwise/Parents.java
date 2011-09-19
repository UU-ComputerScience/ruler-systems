package stepwise;

import java.util.LinkedList;

// the last element of the parents list is the actual node (and of the right type).
public final class Parents<X, I, E> implements Stepwise<X, I, E> {
	private X _syns;
	private LinkedList<Coroutine<I, E>> _stack;

	public Parents(final Node<X, I, E> node, final X syns) {
		_stack = new LinkedList<Coroutine<I, E>>();
		_stack.add(node);
		_syns = syns;
	}

	@Override
	public X lazyEval() {
		_stack.clear();
		return _syns;
	}

	@Override
	public Report<I, E> nextStep() {
		while (true) {
			Coroutine<I, E> head = _stack.poll();
			if (head == null) {
				return new ReportDone<I, E>(); // stack empty, we are done
			}

			if (head instanceof Parents) { // merge stacks
				@SuppressWarnings("unchecked")
				Parents<?, I, E> other = (Parents<?, I, E>) head;
				_stack.addAll(0, other._stack);
				continue;
			}

			Report<I, E> rep = head.nextStep();
			if (rep instanceof ReportReplace) {
				ReportReplace<I, E> repl = (ReportReplace<I, E>) rep;
				Coroutine<I, E> comp = (Coroutine<I, E>) repl.get();
				_stack.addFirst(comp);
				continue;
			} else if (rep instanceof ReportFail) {
				return rep;
			} else if (rep instanceof ReportChild) {
				ReportChild<I, E> child = (ReportChild<I, E>) rep;
				Coroutine<I, E> comp = (Coroutine<I, E>) child.get();
				_stack.addFirst(head);
				_stack.addFirst(comp);
				continue;
			} else if (rep instanceof ReportDone) {
				continue;
			} else if (rep instanceof ReportInfo) {
				_stack.addFirst(head);
				return rep;
			} else {
				throw new RuntimeException(
						"Parents.nextStep(): unexpected report type.");
			}
		}
	}
}

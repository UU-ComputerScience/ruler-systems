package stepwise;

public interface Stepwise<X,I,E> extends Coroutine<I,E> {
	X lazyEval();
}

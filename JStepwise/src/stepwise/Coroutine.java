package stepwise;

public interface Coroutine<I,E> {
	Report<I,E> nextStep();
}

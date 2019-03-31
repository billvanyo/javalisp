package lisp.exceptions;

public abstract class LispException extends Exception {
    public LispException(String message) {
        super(message);
    }
}

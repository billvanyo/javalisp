package lisp.exceptions;

public class InvalidArgumentException extends LispException {
    public InvalidArgumentException(String message) {
        super(message);
    }
}

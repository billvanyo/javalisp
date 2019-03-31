package lisp.exceptions;

public class InvalidFunctionException extends LispException {
    public InvalidFunctionException(String message) {
        super(message);
    }
}

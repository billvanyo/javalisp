package lisp.data;

import lisp.exceptions.InvalidArgumentException;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public abstract class SExpression implements Iterable<SExpression> {

    public abstract SExpression car() throws InvalidArgumentException;

    public abstract SExpression cdr() throws InvalidArgumentException;

    public boolean isAtom() {
        return false;
    }

    public boolean isSymbol() {
        return false;
    }

    public boolean isNumber() {
        return false;
    }

    public boolean isString() {
        return false;
    }

    public abstract void print(PrintStream printStream);

    public String toString() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(baos);
        this.print(ps);
        ps.close();
        return new String(baos.toByteArray(), StandardCharsets.UTF_8);
    }

    public Stream<SExpression> stream() {
        return StreamSupport.stream(this.spliterator(), false);
    }

}

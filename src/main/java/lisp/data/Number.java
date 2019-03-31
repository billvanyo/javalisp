package lisp.data;

import java.io.PrintStream;
import java.util.HashMap;

public class Number extends Atom {
    private static HashMap<Long, Number> numberTable = new HashMap<>();
    public static final Number ZERO = number(0);
    public static final Number ONE = number(1);
    private long number;

    private Number(long number) {
        this.number = number;
    }

    public static Number number(long number) {
        return numberTable.computeIfAbsent(number, Number::new);
    }

    @Override
    public boolean isNumber() {
        return true;
    }

    public void print(PrintStream printStream) {
        printStream.print(number);
    }

    public long value() {
        return this.number;
    }
}

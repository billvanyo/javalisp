package lisp.data;

import java.io.PrintStream;
import java.util.HashMap;

public class LispString extends Atom {
    private static HashMap<String, LispString> stringTable = new HashMap<>();
    private String string;

    private LispString(String string) {
        this.string = string;
    }

    public static LispString string(String string) {
        return stringTable.computeIfAbsent(string, LispString::new);
    }

    @Override
    public boolean isString() {
        return true;
    }

    public void print(PrintStream printStream) {
        printStream.print("\"" + string + "\"");
    }

    public String value() {
        return this.string;
    }
}

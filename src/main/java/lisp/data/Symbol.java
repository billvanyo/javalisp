package lisp.data;

import java.io.PrintStream;
import java.util.HashMap;

public class Symbol extends Atom {
    private static HashMap<String, Symbol> symbolTable = new HashMap<>();
    public static final Symbol NIL = symbol("NIL");
    public static final Symbol T = symbol("T");
    public static final Symbol CAR = symbol("CAR");
    public static final Symbol CDR = symbol("CDR");
    public static final Symbol CONS = symbol("CONS");
    public static final Symbol LIST = symbol("LIST");
    public static final Symbol IF = symbol("IF");
    public static final Symbol COND = symbol("COND");
    public static final Symbol EQ = symbol("EQ");
    public static final Symbol NULL = symbol("NULL");
    public static final Symbol ATOM = symbol("ATOM");
    public static final Symbol SYMBOLP = symbol("SYMBOLP");
    public static final Symbol NUMBERP = symbol("NUMBERP");
    public static final Symbol LAMBDA = symbol("LAMBDA");
    public static final Symbol MACRO = symbol("MACRO");
    public static final Symbol CLOSURE = symbol("CLOSURE");
    public static final Symbol QUOTE = symbol("QUOTE");
    public static final Symbol BACKQUOTE = symbol("BACKQUOTE");
    public static final Symbol COMMA = symbol("COMMA");
    public static final Symbol COMMA_AT = symbol("COMMA_AT");
    public static final Symbol EVAL = symbol("EVAL");
    public static final Symbol APPLY = symbol("APPLY");
    public static final Symbol AND = symbol("AND");
    public static final Symbol OR = symbol("OR");
    public static final Symbol NOT = symbol("NOT");
    public static final Symbol PLUS = symbol("+");
    public static final Symbol MINUS = symbol("-");
    public static final Symbol TIMES = symbol("*");
    public static final Symbol DIV = symbol("DIV");
    public static final Symbol MOD = symbol("MOD");
    public static final Symbol ADD1 = symbol("1+");
    public static final Symbol SUB1 = symbol("1-");
    public static final Symbol ZEROP = symbol("ZEROP");
    public static final Symbol GREATERP = symbol("GREATERP");
    public static final Symbol LESSP = symbol("LESSP");
    public static final Symbol DEFUN = symbol("DEFUN");
    public static final Symbol DEFMACRO = symbol("DEFMACRO");
    public static final Symbol SETQ = symbol("SETQ");
    public static final Symbol UNDEFINED = symbol("UNDEFINED");
    public static final Symbol TRACE = symbol("TRACE");
    public static final Symbol UNTRACE = symbol("UNTRACE");
    public static final Symbol LOAD = symbol("LOAD");

    private String name;

    private Symbol(String name) {
        this.name = name.toUpperCase();
    }

    public static Symbol symbol(String name) {
        return symbolTable.computeIfAbsent(name.toUpperCase(), Symbol::new);
    }

    @Override
    public boolean isSymbol() {
        return true;
    }

    public void print(PrintStream printStream) {
        printStream.print(name);
    }
}

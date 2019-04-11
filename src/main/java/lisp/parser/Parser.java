package lisp.parser;

import lisp.Interpreter;
import lisp.data.Number;
import lisp.data.*;
import lisp.exceptions.LispIOException;
import lisp.exceptions.ParsingException;

import java.io.BufferedReader;

import static lisp.Interpreter.cons;
import static lisp.data.Symbol.*;

public class Parser {

    private LispTokenStream lispTokenStream;

    public Parser(BufferedReader input) throws LispIOException {
        this.lispTokenStream = new LispTokenStream(input);
    }

    public SExpression read() throws ParsingException {
        SExpression expr = readExpression();
        if (expr == null) {
            throw new ParsingException("unexpected ')' token");
        }
        return expr;
    }

    public SExpression readExpression() throws ParsingException {
        String token = lispTokenStream.nextToken();
        if (token == null) return NIL;  // EOF when loading file
        if (".".equals(token)) {
            throw new ParsingException("unexpected '.' token");
        } else if ("(".equals(token)) {
            SExpression car = readExpression();
            if (car == null) return NIL;
            else return cons(car, readCdr());
        } else if (")".equals(token)) {
            return null;
        } else if ("'".equals(token)) {
            return cons(QUOTE, cons(readExpression(), NIL));
        } else if ("`".equals(token)) {
            return cons(BACKQUOTE, cons(readExpression(), NIL));
        } else if (",".equals(token)) {
            return cons(COMMA, cons(readExpression(), NIL));
        } else if (",@".equals(token)) {
            return cons(COMMA_AT, cons(readExpression(), NIL));
        } else if (token.charAt(0) == '"') {
            return makeString(token);
        } else {
            return makeAtom(token);
        }
    }

    private SExpression readCdr() throws ParsingException {
        String token = lispTokenStream.nextToken();
        if (".".equals(token)) {
            SExpression cdr = readExpression();
            token = lispTokenStream.nextToken();
            if (!")".equals(token)) throw new ParsingException("expected ')', found '" + token + "'");
            return cdr;
        } else if ("(".equals(token)) {
            SExpression car = readExpression();
            if (car == null) car = Symbol.NIL;
            else car = Interpreter.cons(car, readCdr());
            return Interpreter.cons(car, readCdr());
        } else if (")".equals(token)) {
            return Symbol.NIL;
        } else if ("'".equals(token)) {
            return cons(cons(QUOTE, cons(readExpression(), NIL)), readCdr());
        } else if ("`".equals(token)) {
            return cons(cons(BACKQUOTE, cons(readExpression(), NIL)), readCdr());
        } else if (",".equals(token)) {
            return cons(cons(COMMA, cons(readExpression(), NIL)), readCdr());
        } else if (",@".equals(token)) {
            return cons(cons(COMMA_AT, cons(readExpression(), NIL)), readCdr());
        } else if (token != null && token.charAt(0) == '"') {
            return cons(makeString(token), readCdr());
        } else {
            return cons(makeAtom(token), readCdr());
        }
    }

    private Atom makeAtom(String token) {
        try {
            long number = Long.parseLong(token);
            return Number.number(number);
        } catch (NumberFormatException e) {
            return Symbol.symbol(token);
        }
    }

    private LispString makeString(String token) {
        return LispString.string(token.substring(1, token.length() - 1));
    }
}

package lisp;

import lisp.data.SExpression;
import lisp.data.Symbol;
import lisp.exceptions.InvalidArgumentException;
import lisp.exceptions.InvalidFunctionException;
import lisp.exceptions.LispException;
import lisp.parser.Parser;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.Iterator;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.*;

class InterpreterTest {

    private static Interpreter interpreter;

    @BeforeEach
    public void setup() {
        interpreter = new Interpreter();
    }

    private static SExpression sexpr(String str) throws LispException {
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        return p.readExpression();
    }

    private static String eval(String exprStr) throws LispException {
        SExpression expr = sexpr(exprStr);
        SExpression value = interpreter.eval(expr);
        return value.toString();
    }

    @Test
    public void factorial_combinator_test() throws LispException {
        String str = "(((LAMBDA (F) ((LAMBDA (X) (F X)) (LAMBDA (X) (F X)))) (LAMBDA (F) (LAMBDA (N) (IF (ZEROP N) 1 (TIMES N ((F F) (SUB1 N))))))) 10)";
        String value = eval(str);

        assertEquals("3628800", value);
    }

    @Test
    public void macro_test() throws LispException {
        String str;
        String value;

        str = "(defun mapcar (f l) (if (nullp l) nil (cons (f (car l)) (mapcar f (cdr l)))))";
        value = eval(str);

        str = "(defun compose (f1 f2) (lambda (x) (f1 (f2 x))))";
        value = eval(str);

        str = "(defmacro let (alist body) `((lambda ,(mapcar 'car alist) ,body) ,@(mapcar (compose 'car 'cdr) alist)))";
        value = eval(str);

        str = "(let ((a 1) (b 2)) (plus a b))";
        value = eval(str);
        assertEquals("3", value);

        str = "(let ((a 3) (b 5)) (let ((x (times a b)) (y (plus a b))) (list x y)))";
        value = eval(str);
        assertEquals("(15 8)", value);
    }

    @Test
    public void upward_funarg_test() throws LispException {
        String str;
        String value;

        str = "(defun recurse (n m) (if (zerop n) (lambda () m) (recurse (sub1 n) (times m n))))";
        value = eval(str);

        str = "(defun fact (m) ((recurse m 1)))";
        value = eval(str);

        str = "(fact 5)";
        value = eval(str);
        assertEquals("120", value);
    }

    @Test
    public void conditional_and_test() throws LispException {
        String str;
        String value;
        // (car 'atom) would throw an exception if evaluated
        str = "(and (zerop 1) (car 'atom))";
        value = eval(str);
        assertEquals("NIL", value);
    }

    @Test
    public void conditional_or_test() throws LispException {
        String str;
        String value;
        // (car 'atom) would throw an exception if evaluated
        str = "(or (zerop 0) (car 'atom))";
        value = eval(str);
        assertEquals("T", value);
    }

    @Test
    public void peano_test() throws LispException {
        String str;
        String value;

        str = "(defun sum (x y) (if (zerop y) x (sum (add1 x) (sub1 y))))";
        value = eval(str);

        str = "(defun prod (x y) (if (zerop y) 0 (sum x (prod x (sub1 y)))))";
        value = eval(str);

        str = "(defun power (x y) (if (zerop y) 1 (prod x (power x (sub1 y)))))";
        value = eval(str);

        str = "(sum 3 3)";
        value = eval(str);
        assertEquals("6", value);

        str = "(prod 3 3)";
        value = eval(str);
        assertEquals("9", value);

        str = "(power 3 3)";
        value = eval(str);
        assertEquals("27", value);
    }

    @Test
    public void exception_test1() throws LispException {
        assertThrows(InvalidArgumentException.class, ()-> eval("(car 'atom)"));
    }

    @Test
    public void exception_test2() throws LispException {
        assertThrows(InvalidArgumentException.class, ()-> eval("(cdr 'atom)"));
    }

    @Test
    public void exception_test3() throws LispException {
        String value;
        assertThrows(InvalidArgumentException.class, ()-> eval("(plus 1 2 3 'atom 4 5 6)"));
        assertThrows(InvalidFunctionException.class, ()-> eval("(1 2 3 'atom 4 5 6)"));
        // test that exception doesn't kill interpreter
        value = eval("(plus 1 2 3 4 5 6)");
        assertEquals("21", value);
    }



}
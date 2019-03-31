package lisp;

import lisp.data.Number;
import lisp.data.*;
import lisp.exceptions.InvalidArgumentException;
import lisp.exceptions.*;
import lisp.parser.Parser;

import java.io.*;
import java.util.Collections;

import static lisp.data.Number.*;
import static lisp.data.Symbol.*;

public class Interpreter {

    private boolean trace = false;
    private SExpression globalEnv = NIL;
    private InputStream inputStream = System.in;
    private PrintStream printStream = System.out;

    private int traceLevel = 0;

    Interpreter() {
    }

    public static void main(String[] args) throws LispException {
        Interpreter interpreter = new Interpreter();
        interpreter.readEvalPrintLoop();
    }

    void readEvalPrintLoop() throws LispException {
        readEvalPrintLoop(inputStream, printStream);
    }

    void readEvalPrintLoop(InputStream inputStream, PrintStream printStream) throws LispException {
        BufferedReader br = new BufferedReader(new InputStreamReader(inputStream));
        Parser parser = new Parser(br);
        SExpression expression;
        SExpression value;
        while (true) {
            try {
                expression = parser.readExpression();
                if (expression == NIL) return;
                expression.print(printStream);
                printStream.println("\n");
                try {
                    value = eval(expression, globalEnv);
                    value.print(printStream);
                    printStream.println("\n");
                } catch (LispException e) {
                    printStream.println("exception: " + e.getMessage() + "\n");
                    //e.printStackTrace();
                }
            } catch (ParsingException e) {
                printStream.println("parsing exception: " + e.getMessage() + "\n");
                //e.printStackTrace();
            }

        }
    }

    public static SExpression cons(SExpression car, SExpression cdr) {
        return new Cons(car, cdr);
    }

    private static SExpression car(SExpression expr) throws InvalidArgumentException { return expr.car(); }
    private static SExpression cdr(SExpression expr) throws InvalidArgumentException { return expr.cdr(); }
    private static SExpression caar(SExpression expr) throws InvalidArgumentException { return car(car(expr)); }
    private static SExpression cadr(SExpression expr) throws InvalidArgumentException { return car(cdr(expr)); }
    private static SExpression cdar(SExpression expr) throws InvalidArgumentException { return cdr(car(expr)); }
    private static SExpression cddr(SExpression expr) throws InvalidArgumentException { return cdr(cdr(expr)); }
    private static SExpression cadar(SExpression expr) throws InvalidArgumentException { return car(cdar(expr)); }
    private static SExpression caddr(SExpression expr) throws InvalidArgumentException { return car(cddr(expr)); }
    private static SExpression cdddr(SExpression expr) throws InvalidArgumentException { return cdr(cddr(expr)); }
    private static SExpression cadddr(SExpression expr) throws InvalidArgumentException { return car(cdddr(expr)); }


    /*
        eval evaluates an expression in the context of an environment, which is an association between symbols and
        their values, represented as a list of dotted pairs (or association list).
     */
    SExpression eval(SExpression expr, SExpression env) throws LispException {
        SExpression value;
        if (trace) traceEvalEntry(expr, env);
        if (expr.isSymbol()) {
            // T and NIL are special symbols that always evaluate to themselves
            if (T == expr || NIL == expr) value = expr;
                // for all other symbols, their value is found in the environment association list
            else value = assoc((Symbol) expr, env);
        } else if (expr.isNumber()) value = expr;    // numbers evaluate to themselves
        else {
            // if not a symbol or number, it's an sexpression (symbolic expression), which should be a list,
            // where the first element (car) is a function (or special form)
            SExpression func = car(expr);
            // special forms are handled first
            // these are exceptions to the general rule (which is: evaluate arguments, apply the function)
            if (func == QUOTE) value = cadr(expr);
            else if (func == BACKQUOTE) value = evalCommas(cadr(expr), env);
            else if (func == IF)
                value = (T == eval(cadr(expr), env)) ? eval(caddr(expr), env) : eval(cadddr(expr), env);
            else if (func == COND) value = evalConditional(cdr(expr), env);
            else if (func == AND) value = evalAnd(cdr(expr), env);
            else if (func == OR) value = evalOr(cdr(expr), env);
            else if (func == SETQ) value = performSetq((Symbol) cadr(expr), eval(caddr(expr), env));
            else if (func == DEFUN) value = performDefun((Symbol) cadr(expr), caddr(expr), cadddr(expr));
            else if (func == DEFMACRO) value = performDefmacro((Symbol) cadr(expr), caddr(expr), cadddr(expr));
            else if (func == LAMBDA) value = cons(CLOSURE, cons(expr, cons(env, NIL)));
            else if (func == LOAD) value = loadFile(cadr(expr));
            else if (func.isSymbol()) {
                SExpression funcVal = assoc((Symbol) func, env);
                if (!funcVal.isAtom() && car(funcVal) == MACRO) {
                    SExpression macroParams = cadr(funcVal);
                    SExpression macroBody = caddr(funcVal);
                    SExpression expandedMacro = expandMacro(macroParams, cdr(expr), macroBody, env);
                    value = eval(expandedMacro, env);
                } else value = apply(func, map((s) -> (eval(s, env)), cdr(expr)), env);
            } else if (func.isAtom()) {
                throw new InvalidFunctionException(func.toString() + " is not a function");
            } else value = apply(func, map((s) -> (eval(s, env)), cdr(expr)), env);
        }
        if (trace) traceEvalReturn(value);
        return value;
    }

    SExpression eval(SExpression expr) throws LispException {
        return eval(expr, this.globalEnv);
    }

    SExpression apply(SExpression function, SExpression argValues, SExpression env) throws LispException {
        SExpression value;
        if (trace) traceApplyEntry(function, argValues, env);
        if (function.isSymbol()) {
            if (function == TRACE) {
                trace = true;
                value = TRACE;
            } else if (function == UNTRACE) {
                trace = false;
                value = UNTRACE;
            } else if (function == CAR) value = car(consCheck(car(argValues)));
            else if (function == CDR) value = cdr(consCheck(car(argValues)));
            else if (function == CONS) value = cons(car(argValues), cadr(argValues));
            else if (function == LIST) value = argValues;
            else if (function == NULLP) value = car(argValues) == NIL ? T : NIL;
            else if (function == EQ) value = car(argValues) == cadr(argValues) ? T : NIL;
            else if (function == ATOMP) value = car(argValues).isAtom() ? T : NIL;
            else if (function == SYMBOLP) value = car(argValues).isSymbol() ? T : NIL;
            else if (function == NUMBERP) value = car(argValues).isNumber() ? T : NIL;
            else if (function == SUB1) value = number(((Number) numberCheck(car(argValues))).value() - 1);
            else if (function == ADD1) value = number(((Number) numberCheck(car(argValues))).value() + 1);
            else if (function == ZEROP) value = car(argValues) == ZERO ? T : NIL;
            else if (function == LESSP) value = (numberValue(car(argValues)) < numberValue(cadr(argValues))) ? T : NIL;
            else if (function == GREATERP) value = (numberValue(car(argValues)) > numberValue(cadr(argValues))) ? T : NIL;
            else if (function == TIMES)
                value = reduce(argValues, (n, m) -> number(numberValue(n) * numberValue(m)), ONE);
            else if (function == PLUS)
                value = reduce(argValues, (n, m) -> number(numberValue(n) + numberValue(m)), ZERO);
            else {
                SExpression fVal = assoc((Symbol) function, env);
                if (fVal == NIL) throw new InvalidFunctionException("undefined function " + function.toString());
                value = apply(fVal, argValues, env);
            }
        } else if (car(function) == LAMBDA)
            value = eval(caddr(function), biMap(Interpreter::cons, cadr(function), argValues, env));
        else if (car(function) == CLOSURE) value = apply(cadr(function), argValues, caddr(function));
        else if (car(function) == MACRO) value = apply(cadr(function), argValues, caddr(function));
            //value = apply(cadr(function), argValues, append(caddr(function), env));

        else {
            SExpression evaledFunction = eval(function, env);
            if (evaledFunction == function) throw new InvalidFunctionException("could not resolve function");
            value = apply(evaledFunction, argValues, env);
        }
        if (trace && value != TRACE) traceApplyReturn(value);
        return value;
    }

    private void traceApplyEntry(SExpression function, SExpression argValues, SExpression env) {
        traceLevel++;
        printStream.print(indent() + "APPLY:\n" + indent() + "\tfunction=");
        function.print(printStream);
        printStream.print("\n" + indent() + "\targValues=");
        argValues.print(printStream);
        printStream.print("\n" + indent() + "\tenv=");
        env.print(printStream);
        printStream.println();
    }

    private void traceApplyReturn(SExpression value) {
        printStream.print(indent() + "APPLY:\n" + indent() + "\tvalue=");
        value.print(printStream);
        printStream.println();
        traceLevel--;
    }

    private void traceEvalEntry(SExpression expr, SExpression env) {
        traceLevel++;
        printStream.print(indent() + "EVAL:\n" + indent() + "\texpr=");
        expr.print(printStream);
        printStream.print("\n" + indent() + "\tenv=");
        env.print(printStream);
        printStream.println();
    }

    private void traceEvalReturn(SExpression value) {
        printStream.print(indent() + "EVAL:\n" + indent() + "\tvalue=");
        value.print(printStream);
        printStream.println();
        traceLevel--;
    }

    private String indent() {
        return String.join("", Collections.nCopies(traceLevel, " "));
    }

    private SExpression assoc(Symbol s, SExpression alist) throws InvalidArgumentException {
        if (alist == NIL) return NIL;
        else if (caar(alist) == s) return cdar(alist);
        else return assoc(s, cdr(alist));
    }

    private SExpression append(SExpression list1, SExpression list2) throws InvalidArgumentException {
        return list1 == NIL ? list2 : cons(car(list1), append(cdr(list1), list2));
    }

    private SExpression evalConditional(SExpression conds, SExpression env) throws LispException {
        if (NIL == conds) return NIL;
        else if (T == eval(caar(conds), env)) return eval(cadar(conds), env);
        else return evalConditional(cdr(conds), env);
    }

    private SExpression evalAnd(SExpression args, SExpression env) throws LispException {
        if (NIL == args) return T;
        else if (eval(car(args), env) == NIL) return NIL;
        else return evalAnd(cdr(args), env);
    }

    private SExpression evalOr(SExpression args, SExpression env) throws LispException {
        if (NIL == args) return NIL;
        else if (eval(car(args), env) != NIL) return T;
        else return evalOr(cdr(args), env);
    }

    private SExpression evalCommas(SExpression expr, SExpression env) throws LispException {
        if (expr.isAtom()) return expr;
        else if (car(expr) == COMMA) return eval(cadr(expr), env);
        else if (!car(expr).isAtom() && caar(expr) == COMMA_AT) return append(eval(cadar(expr), env), evalCommas(cdr(expr), env));
        else return cons(evalCommas(car(expr), env), evalCommas(cdr(expr), env));
    }

    private SExpression expandMacro(SExpression params, SExpression argExprs, SExpression body, SExpression env) throws LispException {
        if (trace) traceExpandMacroEntry(params, argExprs, body, env);
        SExpression value = eval(body, biMap(Interpreter::cons, params, argExprs, env));
        if (trace) traceExpandMacroReturn(value);
        return value;
    }

    private void traceExpandMacroEntry(SExpression params, SExpression argExprs, SExpression body, SExpression env) {
        traceLevel++;
        printStream.print(indent() + "EXPAND MACRO:\n" + indent() + "\tparams=");
        params.print(printStream);
        printStream.print("\n" + indent() + "\targExprs=");
        argExprs.print(printStream);
        printStream.print("\n" + indent() + "\tbody=");
        body.print(printStream);
        printStream.print("\n" + indent() + "\tenv= ");
        env.print(printStream);
        printStream.println();
    }

    private void traceExpandMacroReturn(SExpression value) {
        printStream.print(indent() + "EXPAND MACRO:\n" + indent() + "\tvalue=");
        value.print(printStream);
        printStream.println();
        traceLevel--;
    }

    private SExpression performSetq(Symbol sym, SExpression val) throws InvalidArgumentException {
        globalEnv = updateEnv(sym, val, globalEnv);
        return val;
    }

    private SExpression performDefun(Symbol fname, SExpression vars, SExpression body) throws InvalidArgumentException {
        SExpression lambdaExpr = cons(LAMBDA, cons(vars, cons(body, NIL)));
        globalEnv = updateEnv(fname, lambdaExpr, globalEnv);
        return lambdaExpr;
    }

    private SExpression performDefmacro(Symbol fname, SExpression vars, SExpression body) throws InvalidArgumentException {
        SExpression macroDef = cons(MACRO, cons(vars, cons(body, NIL)));
        globalEnv = updateEnv(fname, macroDef, globalEnv);
        return macroDef;
    }

    private SExpression updateEnv(Symbol sym, SExpression val, SExpression env) throws InvalidArgumentException {
        if (env == NIL) return cons(cons(sym, val), NIL);
        else if (sym == caar(env)) return cons(cons(sym, val), cdr(env));
        else return cons(car(env), updateEnv(sym, val, cdr(env)));
    }

    private SExpression map(CheckedFunction<SExpression, SExpression> f, SExpression list) throws LispException {
        if (list == NIL) return NIL;
        else return cons(f.apply(car(list)), map(f, cdr(list)));
    }

    private SExpression biMap(CheckedBiFunction<SExpression, SExpression, SExpression> f, SExpression list1, SExpression list2, SExpression env) throws LispException {
        if (list1 == NIL || list2 == NIL) return env;
        else return cons(f.apply(car(list1), car(list2)), biMap(f, cdr(list1), cdr(list2), env));
    }

    private SExpression reduce(SExpression list, CheckedBiFunction<SExpression, SExpression, SExpression> f, SExpression identity) throws LispException {
        if (list == NIL) return identity;
        else return f.apply(car(list), reduce(cdr(list), f, identity));
    }

    private SExpression consCheck(SExpression argVal) throws LispException {
        if (argVal.isAtom()) throw new InvalidArgumentException("atomic argument: " + argVal.toString());
        else return argVal;
    }

    private SExpression numberCheck(SExpression argVal) throws LispException {
        if (!argVal.isNumber()) throw new InvalidArgumentException("non-numeric argument: " + argVal.toString());
        else return argVal;
    }

    private long numberValue(SExpression argVal) throws LispException {
        if (!argVal.isNumber()) throw new InvalidArgumentException("non-numeric argument: " + argVal.toString());
        else return ((Number)argVal).value();
    }

    @FunctionalInterface
    public interface CheckedFunction<T, R> {
        R apply(T t) throws LispException;
    }

    @FunctionalInterface
    public interface CheckedBiFunction<T, U, R> {
        R apply(T t, U u) throws LispException;
    }

    private SExpression loadFile(SExpression filename) throws LispException {
        if (!filename.isString()) throw new InvalidArgumentException("filename must be a string");
        String filenameString = ((LispString) filename).value();
        InputStream is;
        try {
            is = new FileInputStream(filenameString);
        } catch (FileNotFoundException e) {
            throw new LispIOException("file not found: " + filenameString);
        }
        readEvalPrintLoop(is, printStream);
        return T;
    }
}

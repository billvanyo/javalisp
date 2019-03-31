package lisp.data;

import lisp.exceptions.InvalidArgumentException;

import java.io.PrintStream;
import java.util.Iterator;

import static lisp.data.Symbol.NIL;

public class Cons extends SExpression {
    private SExpression car;
    private SExpression cdr;

    public Cons(SExpression car, SExpression cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public SExpression car() {
        return car;
    }

    public SExpression cdr() {
        return cdr;
    }

    public void print(PrintStream printStream) {
        printStream.print("(");
        SExpression next = this;
        do {
            ((Cons)next).car().print(printStream);
            next = ((Cons)next).cdr();
            if (next.isAtom()) {
                if (next != NIL) {
                    printStream.print(" . ");
                    next.print(printStream);
                }
                printStream.print(")");
            } else {
                printStream.print(" ");
            }
        } while (!next.isAtom());
    }

    @Override
    public Iterator<SExpression> iterator() {
        return new LispListIterator(this);
    }

    class LispListIterator implements Iterator<SExpression> {

        private SExpression current;

        LispListIterator(Cons lispList) {
            this.current = lispList;
        }

        @Override
        public boolean hasNext() {
            return (!current.isAtom());
        }

        @Override
        public SExpression next() {
            SExpression next = null;
            try {
                next = current.car();
                current = current.cdr();
            } catch (InvalidArgumentException e) {
                return null;
            }
            return next;
        }
    }
}

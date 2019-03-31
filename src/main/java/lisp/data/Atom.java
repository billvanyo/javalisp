package lisp.data;

import lisp.exceptions.InvalidArgumentException;

import java.util.Iterator;

public abstract class Atom extends SExpression {

    public boolean isAtom() {
        return true;
    }

    public SExpression car() throws InvalidArgumentException {
        //return Symbol.UNDEFINED;
        throw new InvalidArgumentException("car of atom " + this.toString());
    }

    public SExpression cdr() throws InvalidArgumentException {
        // return Symbol.UNDEFINED;
        throw new InvalidArgumentException("cdr of atom " + this.toString());
    }

    @Override
    public Iterator<SExpression> iterator() {
        return null;
    }
}

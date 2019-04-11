package lisp.data;

import static org.junit.jupiter.api.Assertions.*;

import lisp.exceptions.LispException;
import lisp.parser.Parser;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.Iterator;

public class ConsTest {

    @Test
    public void iterate_through_list() throws LispException {
        String listString = "(a b c)";
        Parser p = new Parser(new BufferedReader(new StringReader(listString)));
        SExpression lispList = p.read();

        Iterator<SExpression> iterator = lispList.iterator();

        assertTrue(iterator.hasNext());
        assertEquals(iterator.next(), Symbol.symbol("a"));
        assertTrue(iterator.hasNext());
        assertEquals(iterator.next(), Symbol.symbol("b"));
        assertTrue(iterator.hasNext());
        assertEquals(iterator.next(), Symbol.symbol("c"));
        assertFalse(iterator.hasNext());
    }
}
package lisp.parser;

import lisp.exceptions.LispException;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.*;

class ParserTest {

    @Test
    void empty_list() throws LispException {
        String str = "()";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("NIL", parsed);
    }

    @Test
    void nil() throws LispException {
        String str = "NIL";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("NIL", parsed);
    }

    @Test
    void dotted_pair1() throws LispException {
        String str = "(a . b)";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("(A . B)", parsed);
    }

    @Test
    void dotted_pair2() throws LispException {
        String str = "(a . nil)";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("(A)", parsed);
    }

    @Test
    void dotted_pair3() throws LispException {
        String str = "(a . (b c))";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("(A B C)", parsed);
    }

    @Test
    void dotted_pair4() throws LispException {
        String str = "(a b c . d))";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("(A B C . D)", parsed);
    }

    @Test
    void quote() throws LispException {
        String str = "'(a b)";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals("(QUOTE (A B))", parsed);
    }

    @Test
    void macro_syntax() throws LispException {
        String str = "(defmacro let (alist body) `((lambda ,(mapcar 'car alist) ,body) ,@(mapcar (compose 'car 'cdr) alist)))";
        String expected = "(DEFMACRO LET (ALIST BODY) (BACKQUOTE ((LAMBDA (COMMA (MAPCAR (QUOTE CAR) ALIST)) (COMMA BODY)) (COMMA_AT (MAPCAR (COMPOSE (QUOTE CAR) (QUOTE CDR)) ALIST)))))";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals(expected, parsed);
    }

    @Test
    void strings() throws LispException {
        // (abc 123 "this is a string" 456 "" "   " "." "123")
        String str = "(abc 123 \"this is a string\" 456 \"\" \"   \" \".\" \"123\")";
        // (ABC 123 "this is a string" 456 "" "   " "." "123")
        String expected = "(ABC 123 \"this is a string\" 456 \"\" \"   \" \".\" \"123\")";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals(expected, parsed);
    }

    @Test
    void ignore_comments() throws LispException {
        String str = "(a b c d) ; this is a comment";
        String expected = "(A B C D)";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals(expected, parsed);
    }

    @Test
    void ignore_comments_multiline() throws LispException {
        String str = "(a b c ; this is a comment\n;comment d e)\n1 2)";
        String expected = "(A B C 1 2)";
        Parser p = new Parser(new BufferedReader(new StringReader(str)));
        String parsed = p.read().toString();
        assertEquals(expected, parsed);
    }


}
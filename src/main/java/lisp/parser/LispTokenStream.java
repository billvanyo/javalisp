package lisp.parser;

import lisp.exceptions.LispIOException;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.StringTokenizer;


class LispTokenStream {

    private BufferedReader input;
    private StringTokenizer stringTokenizer;
    private String line;
    private String delimiters = " \t()'`,@;\"";
    private String bufferedToken = null;

    LispTokenStream(BufferedReader input) throws LispIOException {
        this.input = input;
        try {
            line = input.readLine();
        } catch (IOException e) {
            throw new LispIOException(e.getMessage());
        }
        stringTokenizer = new StringTokenizer(line, delimiters, true);
    }

    String nextToken() {
        String token = getNextTokenWithDelimiters();
        while (" ".equals(token) || "\t".equals(token) || ";".equals(token)) {
            if (";".equals(token)) {
                try {
                    line = input.readLine();
                    stringTokenizer = new StringTokenizer(line, delimiters, true);
                } catch (IOException e) {
                    return null;
                }
            }
            token = getNextTokenWithDelimiters();
        }
        if (",".equals(token)) {
            String nextToken = getNextTokenWithDelimiters();
            if ("@".equals(nextToken)) return ",@";
            else {
                bufferedToken = nextToken;
                return ",";
            }
        } else if ("\"".equals(token)) {
            // NOTE: there's no character escape mechanism; strings can't contain double quote
            String string = "\"";
            while (!"\"".equals(token = getNextTokenWithDelimiters())) {
                string = string + token;
            }
            string = string + token;
            return string;
        }
        return token;
    }

    private String getNextTokenWithDelimiters() {
        try {
            while (!stringTokenizer.hasMoreElements() && bufferedToken == null) {
                line = input.readLine();
                if (line == null) return null;
                stringTokenizer = new StringTokenizer(line, delimiters, true);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        if (bufferedToken != null) {
            String token = bufferedToken;
            bufferedToken = null;
            return token;
        } else return stringTokenizer.nextToken();
    }

}

package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.special.SlartiSpecialForm;
import de.weltraumschaf.slartibartfass.node.type.*;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

final class Parser {

    List<SlartiNode> read(final InputStream input) throws IOException {
        return read(new Reader(input));
    }

    private List<SlartiNode> read(final Reader input) throws IOException {
        final List<SlartiNode> nodes = new ArrayList<SlartiNode>();

        do {
            readWhitespace(input);

            if (input.peek() == ';') {
                readComments(input);
            }

            if (input.isEof()) {
                break;
            }

            if (input.peek() == ';') {
                // Catch multi line comments.
                continue;
            }

            nodes.add(readNode(input));
        } while (true);

        return nodes;
    }

    private  void readWhitespace(final Reader input) throws IOException {
        do {
            if (input.isEof()) {
                break;
            }

            if (Character.isWhitespace(input.peek())) {
                input.next(); // Consume the whitespace.
            } else {
                break;
            }
        } while (true);
    }

    private  void readComments(final Reader input) throws IOException {
        assertCharacter(input.next(), ';', "Reading a comment must start with ';'");

        do {
            if (input.isEof()) {
                break;
            }

            if ('\n' == input.next()) {
                break;
            }
        } while (true);
    }

    private SlartiNode readNode(final Reader input) throws IOException {
        readWhitespace(input);
        final char c = input.peek();

        if (c == '(') {
            return readList(input);
        } else if (Character.isDigit(c)) {
            return readNumber(input);
        } else if (c == '#') {
            return readBoolean(input);
        } else if (c == '"') {
            return readString(input);
        } else if (c == ')') {
            throw syntaxError("Unmatched close paren");
        } else {
            return readSymbol(input);
        }
    }

    private SlartiNode readList(final Reader input) throws IOException {
        final char first = input.next();
        assertCharacter(first, '(', "Reading a list must start with '('");
        final List<SlartiNode> list = new ArrayList<>();

        do {
            readWhitespace(input);
            char c = input.peek();

            if (c == ')') {
                input.next(); // Consume ')', end of list.
                break;
            } else if (input.isEof()) {
                throw syntaxError("EOF reached before closing of list.");
            } else {
                list.add(readNode(input));
            }
        } while (true);

        return SlartiSpecialForm.check(new SlartiList(list));
    }

    private SlartiNode readNumber(final Reader input) throws IOException {
        final StringBuilder buffer = new StringBuilder();

        do {
            if (input.isEof()) {
                break;
            }

            if (Character.isDigit(input.peek())) {
                buffer.append(input.next());
            } else {
                break;
            }
        } while (true);

        return new SlartiNumber(Long.parseLong(buffer.toString()));
    }

    private SlartiNode readBoolean(final Reader input) throws IOException {
        final char first = (char) input.next();
        assertCharacter(first, '#', "Reading a boolean must start with '#'");
        final StringBuilder buffer = new StringBuilder();

        do {
            if (input.isEof()) {
                break;
            }

            final char c = input.peek();

            if (c == '#' || Character.isAlphabetic(c)) {
                buffer.append(input.next());
            } else {
                break;
            }
        } while (true);

        switch (buffer.toString()) {
            case "true": return SlartiBoolean.TRUE;
            case "false": return SlartiBoolean.FALSE;
            default: throw unexpectedTokenError(buffer.toString());
        }
    }

    private SlartiNode readSymbol(final Reader input) throws IOException {
        final StringBuilder buffer = new StringBuilder();

        do {
            if (input.isEof()) {
                break;
            }

            final char c = (char) input.peek();

            if (Character.isWhitespace(c) || c == ')') {
                break;
            } else {
                buffer.append(input.next());
            }
        } while (true);

        return new SlartiSymbol(buffer.toString());
    }

    private SlartiNode readString(final Reader input) throws IOException {
        final char first = (char) input.next();
        assertCharacter(first, '"', "Reading a list must start with '\"'");
        final StringBuilder buffer = new StringBuilder();

        do {
            if (input.peek() == '"') {
                input.next(); // End of string, consume ".
                break;
            } else if (input.isEof()) {
                throw syntaxError("EOF reached before closing of string.");
            } else {
                buffer.append(input.next());
            }
        } while (true);

        return new SlartiString(buffer.toString());
    }

    private SyntaxError unexpectedTokenError(final String token) {
        return syntaxError(String.format("Unrecognizable token '%s'", token));
    }

    private SyntaxError syntaxError(final String msg) {
        return new SyntaxError(String.format("Syntax error: %s!", msg));
    }

    private void assertCharacter(final char c, final char expected, final String msg) {
        if (c != expected) {
            throw syntaxError(msg);
        }
    }
}

package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.special.SlartiSpecialForm;
import de.weltraumschaf.slartibartfass.node.type.*;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

final class Reader {

    List<SlartiNode> read(final InputStream input) throws IOException {
        return read(new PushbackReader(new InputStreamReader(input)));
    }

    private List<SlartiNode> read(final PushbackReader input) throws IOException {
        final List<SlartiNode> nodes = new ArrayList<SlartiNode>();

        readWhitespace(input);
        readComments(input);
        char c = (char) input.read();

        while (isNotEof(c)) {
            input.unread(c);
            nodes.add(readNode(input));
            readWhitespace(input);
            readComments(input);
            c = (char) input.read();
        }

        return nodes;
    }

    private  void readWhitespace(final PushbackReader input) throws IOException {
        do {
            final char c = (char) input.read();

            if (isEof(c)) {
                break;
            }

            if (!Character.isWhitespace(c)) {
                input.unread(c);
                break;
            }
        } while (true);
    }

    private  void readComments(final PushbackReader input) throws IOException {
        char c = (char) input.read();

        if (';' != c) {
            input.unread(c);
            return;
        }

        do {
            c = (char) input.read();

            if (isEof(c)) {
                break;
            }

            if ('\n' == c) {
                break;
            }
        } while (true);
    }

    private SlartiNode readNode(final PushbackReader input) throws IOException {
        char c = (char) input.read();
        input.unread(c);

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

    private SlartiNode readList(final PushbackReader input) throws IOException {
        final char first = (char) input.read();
        assertCharacter(first, '(', "Reading a list must start with '('");
        final List<SlartiNode> list = new ArrayList<>();

        do {
            readWhitespace(input);
            char c = (char) input.read();

            if (c == ')') {
                // end of list
                break;
            } else if (isEof(c)) {
                throw syntaxError("EOF reached before closing of list.");
            } else {
                input.unread(c);
                list.add(readNode(input));
            }
        } while (true);

        return SlartiSpecialForm.check(new SlartiList(list));
    }

    private SlartiNode readNumber(final PushbackReader input) throws IOException {
        final StringBuilder buffer = new StringBuilder();

        do {
            final char c = (char) input.read();

            if (isEof(c)) {
                break;
            }

            if (Character.isDigit(c)) {
                buffer.append(c);
            } else {
                input.unread(c);
                break;
            }
        } while (true);

        return new SlartiNumber(Long.parseLong(buffer.toString()));
    }

    private SlartiNode readBoolean(final PushbackReader input) throws IOException {
        final char first = (char) input.read();
        assertCharacter(first, '#', "Reading a boolean must start with '#'");
        final StringBuilder buffer = new StringBuilder();

        do {
            final char c = (char) input.read();

            if (isEof(c)) {
                break;
            }

            if (c == '#' || Character.isAlphabetic(c)) {
                buffer.append(c);
            } else {
                input.unread(c);
                break;
            }
        } while (true);

        switch (buffer.toString()) {
            case "true": return SlartiBoolean.TRUE;
            case "false": return SlartiBoolean.FALSE;
            default: throw unexpectedTokenError(buffer.toString());
        }
    }

    private SlartiNode readSymbol(final PushbackReader input) throws IOException {
        final StringBuilder buffer = new StringBuilder();

        do {
            final char c = (char) input.read();

            if (isEof(c)) {
                break;
            }

            if (Character.isWhitespace(c) || c == ')') {
                input.unread(c);
                break;
            } else {
                buffer.append(c);
            }
        } while (true);

        return new SlartiSymbol(buffer.toString());
    }

    private SlartiNode readString(final PushbackReader input) throws IOException {
        final char first = (char) input.read();
        assertCharacter(first, '"', "Reading a list must start with '\"'");
        final StringBuilder buffer = new StringBuilder();

        do {
            char c = (char) input.read();

            if (c == '"') {
                // end of string
                break;
            } else if (isEof(c)) {
                throw syntaxError("EOF reached before closing of string.");
            } else {
                buffer.append(c);
            }
        } while (true);

        return new SlartiString(buffer.toString());
    }

    private boolean isNotEof(final char c) {
        return !isEof(c);
    }

    private boolean isEof(final char c) {
        return (byte) c == -1;
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

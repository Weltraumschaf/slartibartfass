package de.weltraumschaf.slartibartfass;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PushbackReader;

public final class Reader {

    private final PushbackReader input;

    public Reader(final InputStream input) {
        super();
        this.input = new PushbackReader(new InputStreamReader(input));
    }

    public char next() throws IOException {
        return (char) input.read();
    }

    public char peek() throws IOException {
        final char c = (char) input.read();
        input.unread(c);
        return c;
    }

    public boolean isEof() throws IOException {
        return (byte) peek() == -1;
    }
}

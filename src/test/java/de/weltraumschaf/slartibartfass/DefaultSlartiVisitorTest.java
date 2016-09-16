package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.Parser;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;

import static org.mockito.Mockito.mock;

public class SlartiVisitorTest {
    private final Parsers parsers = new Parsers(mock(PrintStream.class), mock(PrintStream.class));
    private final SlartiVisitor sut = new SlartiVisitor();

    private InputStream createSrc(final String src) {
        return new ByteArrayInputStream(src.getBytes());
    }

    @Test
    public void foo() throws IOException {
        final SlartiParser parser = parsers.newParser(createSrc(""), false);

        final SlartiNode node = sut.visit(parser.file());
    }
}

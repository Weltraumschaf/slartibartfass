package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.SlartiInputOutput;
import de.weltraumschaf.slartibartfass.frontend.Parsers;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser.FileContext;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.InOrder;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

/**
 * Tests for {@link Interpreter}.
 *
 * @author Sven Strittmatter
 */
public class InterpreterTest {

    private final Interpreter sut = new Interpreter(
        new SlartiInputOutput(mock(IO.class)), Arrays.asList("foo", "bar", "baz"));

    @Test
    @Ignore
    @SuppressWarnings("unchecked")
    public void start() throws IOException {
        final Parsers parsers = mock(Parsers.class);
        sut.parsers(parsers);
        final SlartiVisitor<SlartiNode> visitor = mock(SlartiVisitor.class);
        sut.visitor(visitor);
        final SlartiParser parser = mock(SlartiParser.class);
        doReturn(mock(FileContext.class)).when(parser).file();
        when(parsers.newParser(any(InputStream.class))).thenReturn(parser);

        sut.start();

        final InOrder inOrder = inOrder(parsers, parser, visitor);
        inOrder.verify(parsers, times(1)).newParser(new FileInputStream("foo"));
        inOrder.verify(parsers, times(1)).newParser(new FileInputStream("bar"));
        inOrder.verify(parsers, times(1)).newParser(new FileInputStream("baz"));
    }
}

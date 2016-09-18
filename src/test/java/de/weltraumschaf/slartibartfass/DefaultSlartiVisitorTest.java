package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import de.weltraumschaf.slartibartfass.node.special.DefineSpecialForm;
import de.weltraumschaf.slartibartfass.node.special.QuoteSpecialForm;
import de.weltraumschaf.slartibartfass.node.type.*;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

@SuppressWarnings("unchecked")
public class DefaultSlartiVisitorTest {
    private final Parsers parsers = new Parsers(mock(IO.class));
    private final DefaultSlartiVisitor sut = new DefaultSlartiVisitor();

    private InputStream stream(final String input) {
        return new ByteArrayInputStream(input.getBytes());
    }

    private SlartiParser parser(final String inout) throws IOException {
        return parsers.newParser(stream(inout), false);
    }

    @Test
    public void visit_empty() throws IOException {
        final SlartiNode nodes = sut.visit(parser("").file());

        assertThat(
            nodes,
            is(SlartiList.EMPTY));
    }

    @Test
    public void visit_blank() throws IOException {
        final SlartiNode nodes = sut.visit(parser("   \n   \t     ").file());

        assertThat(
            nodes,
            is(SlartiList.EMPTY));
    }

    @Test
    public void visit_oneNumber() throws IOException {
        final SlartiNode nodes = sut.visit(parser("42").file());

        assertThat(
            nodes,
            is(new SlartiList((new SlartiInteger(42L)))));
    }

    @Test
    public void visit_threeNumbers() throws IOException {
        final SlartiNode nodes = sut.visit(parser(" 42   23  3  ").file());

        assertThat(
            nodes,
            is(
                new SlartiList(new SlartiInteger(42L), new SlartiInteger(23L), new SlartiInteger(3L))
            )
        );
    }

    @Test
    public void visit_true() throws IOException {
        final SlartiNode nodes = sut.visit(parser("#true").file());

        assertThat(
            nodes,
            is(new SlartiList((SlartiBoolean.TRUE))));
    }

    @Test
    public void visit_false() throws IOException {
        final SlartiNode nodes = sut.visit(parser("#false").file());

        assertThat(
            nodes,
            is(new SlartiList(SlartiBoolean.FALSE)));
    }

    @Test
    public void visit_multipleBooleans() throws IOException {
        final SlartiNode nodes = sut.visit(parser(" #true\n #false \t#true").file());

        assertThat(
            nodes,
            is(new SlartiList(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)));
    }

    @Test
    public void visit_oneSymbol() throws IOException {
        final SlartiNode nodes = sut.visit(parser("snafu").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiSymbol("snafu"))));
    }

    @Test
    public void visit_threeSymbol() throws IOException {
        final SlartiNode nodes = sut.visit(parser("  foo  \n bar  \t baz  ").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiSymbol("foo"), new SlartiSymbol("bar"), new SlartiSymbol("baz"))));
    }

    @Test
    public void visit_oneString() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"foobar\"").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiString("foobar"))));
    }

    @Test
    public void visit_oneStringWithWhitespaces() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"  foobar\n\"").file());

        assertThat(
            nodes,
            is(new SlartiList(
                new SlartiString("  foobar\n"))));
    }

    @Test
    public void visit_threeStrings() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"foo\" \"bar\" \"baz\"").file());

        assertThat(
            nodes,
            is(new SlartiList(
                new SlartiString("foo"), new SlartiString("bar"), new SlartiString("baz"))));
    }

    @Test
    public void visit_helloWorldQuote() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(println (quote hello-world!))").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiList(
                new SlartiSymbol("println"), new QuoteSpecialForm(
                    new SlartiList(
                        new SlartiSymbol("hello-world!")
                    ))
            )))
        );
    }

    @Test
    public void visit_helloWorldString() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(println \"Hello, World!\")").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiList(
                new SlartiSymbol("println"),  new SlartiString("Hello, World!")
            ))));
    }

    @Test
    public void visit_helloWorldStringWithComments() throws IOException {
        final SlartiNode nodes = sut.visit(parser(
            " ; One line comment with own line.\n" +
                "(println \"Hello, World!\") ; Comment at end of line\n").file());

        assertThat(
            nodes,
            is(new SlartiList(new SlartiList(
                new SlartiSymbol("println"),  new SlartiString("Hello, World!")
            ))));
    }

    @Test
    public void visit_ignoreComments() throws IOException {
        final String src = "(define n 10)\n" +
            "\n" +
            ";(if (> n 0) \n" +
            ";    (println \"true\") \n" +
            ";    (println \"false\"))\n" +
            "\n" +
            "(foo n)";

        final SlartiNode nodes = sut.visit(parser(src).file());

        assertThat(
            nodes,
            is(
                new SlartiList(
                    new DefineSpecialForm(new SlartiList(new SlartiSymbol("n"), new SlartiInteger(10L))),
                    new SlartiList(new SlartiSymbol("foo"), new SlartiSymbol("n"))
                )
            )
        );
    }
}

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

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

@SuppressWarnings("unchecked")
public class DefaultSlartiVisitorTest {
    private final Parsers parsers = new Parsers(mock(IO.class));
    private final DefaultSlartiVisitor sut = new DefaultSlartiVisitor();

    private InputStream stream(final String input) {
        return new ByteArrayInputStream(input.getBytes());
    }


    private SlartiParser parser(final String input) throws IOException {
        return parser(stream(input));
    }

    private SlartiParser parser(final InputStream input) throws IOException {
        return parsers.newParser(input, false);
    }

    @Test
    public void visit_empty() throws IOException {
        final SlartiNode nodes = sut.visit(parser("").file());

        assertThat(
            nodes,
            is(SlartiList.NIL));
    }

    @Test
    public void visit_blank() throws IOException {
        final SlartiNode nodes = sut.visit(parser("   \n   \t     ").file());

        assertThat(
            nodes,
            is(SlartiList.NIL));
    }

    @Test
    public void visit_oneNumber() throws IOException {
        final SlartiNode nodes = sut.visit(parser("42").file());

        assertThat(
            nodes,
            is(list(of(42L))));
    }

    @Test
    public void visit_threeNumbers() throws IOException {
        final SlartiNode nodes = sut.visit(parser(" 42   23  3  ").file());

        assertThat(nodes, is(list(of(42L), of(23L), of(3L))));
    }

    @Test
    public void visit_true() throws IOException {
        final SlartiNode nodes = sut.visit(parser("#true").file());

        assertThat(nodes, is(list(of(true))));
    }

    @Test
    public void visit_false() throws IOException {
        final SlartiNode nodes = sut.visit(parser("#false").file());

        assertThat(nodes, is(list(of(false))));
    }

    @Test
    public void visit_multipleBooleans() throws IOException {
        final SlartiNode nodes = sut.visit(parser(" #true\n #false \t#true").file());

        assertThat(nodes, is(list(of(true), of(false), of(true))));
    }

    @Test
    public void visit_oneSymbol() throws IOException {
        final SlartiNode nodes = sut.visit(parser("snafu").file());

        assertThat(nodes, is(list(sym("snafu"))));
    }

    @Test
    public void visit_threeSymbol() throws IOException {
        final SlartiNode nodes = sut.visit(parser("  foo  \n bar  \t baz  ").file());

        assertThat(nodes, is(list(sym("foo"), sym("bar"), sym("baz"))));
    }

    @Test
    public void visit_oneString() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"foobar\"").file());

        assertThat(nodes, is(list(of("foobar"))));
    }

    @Test
    public void visit_oneStringWithWhitespaces() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"  foobar\n\"").file());

        assertThat(nodes, is(list(of("  foobar\n"))));
    }

    @Test
    public void visit_threeStrings() throws IOException {
        final SlartiNode nodes = sut.visit(parser("\"foo\" \"bar\" \"baz\"").file());

        assertThat(nodes, is(list(of("foo"), of("bar"), of("baz"))));
    }

    @Test
    public void visit_helloWorldQuote() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(println (quote hello-world!))").file());

        assertThat(nodes,
            is(list(list(
                sym("println"), new QuoteSpecialForm(list(sym("hello-world!")))
            )))
        );
    }

    @Test
    public void visit_helloWorldString() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(println \"Hello, World!\")").file());

        assertThat(nodes,
            is(list(list(
                sym("println"),  of("Hello, World!")
            ))));
    }

    @Test
    public void visit_helloWorldStringWithComments() throws IOException {
        final SlartiNode nodes = sut.visit(parser(
            " ; One line comment with own line.\n" +
                "(println \"Hello, World!\") ; Comment at end list line\n").file());

        assertThat(nodes,
            is(list(list(
                sym("println"),  of("Hello, World!")
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
                list(
                    new DefineSpecialForm(
                        list(sym("n"), of(10L))), list(sym("foo"), sym("n"))
                )
            )
        );
    }

    @Test
    public void visit_stdlib() throws IOException {
        final SlartiParser parser = parser(getClass().getResourceAsStream(Application.BASE_PACKAGE + "/std-lib.sl"));

        final SlartiNode nodes = sut.visit(parser.file());

        assertThat(nodes, is(not(nullValue())));
        assertThat(nodes, is(instanceOf(SlartiList.class)));

        final SlartiList list = (SlartiList) nodes;
        assertThat(list.size(), is(6));
    }

    @Test
    public void visit_literalQuote() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(define (quote a) (quote b))").file());

        assertThat(nodes, is(list(new DefineSpecialForm(list(
            new QuoteSpecialForm(list(sym("a"))),
            new QuoteSpecialForm(list(sym("b"))))))));
    }

    @Test
    public void visit_quote() throws IOException {
        final SlartiNode nodes = sut.visit(parser("(define 'a 'b)").file());

        assertThat(nodes, is(list(new DefineSpecialForm(list(
            new QuoteSpecialForm(list(sym("a"))),
            new QuoteSpecialForm(list(sym("b"))))))));
    }
}

package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.special.DefineSpecialForm;
import de.weltraumschaf.slartibartfass.node.special.QuoteSpecialForm;
import de.weltraumschaf.slartibartfass.node.type.*;
import org.junit.Ignore;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

public class ParserTest {
    private final Parser sut = new Parser();

    private InputStream stream(final String input) {
        return new ByteArrayInputStream(input.getBytes());
    }

    @Test
    public void read_empty() throws IOException {
        assertThat(sut.read(stream("")), hasSize(0));
    }

    @Test
    public void read_blank() throws IOException {
        assertThat(sut.read(stream("   \n   \t     ")), hasSize(0));
    }

    @Test
    public void read_oneNumber() throws IOException {
        assertThat(sut.read(stream("42")), contains(new SlartiNumber(42L)));
    }

    @Test
    public void read_threeNumbers() throws IOException {
        assertThat(
            sut.read(stream(" 42   23  3  ")),
            contains(new SlartiNumber(42L), new SlartiNumber(23L), new SlartiNumber(3L)));
    }

    @Test
    public void read_true() throws IOException {
        assertThat(sut.read(stream("#true")), contains(SlartiBoolean.TRUE));
    }

    @Test
    public void read_false() throws IOException {
        assertThat(sut.read(stream("#false")), contains(SlartiBoolean.FALSE));
    }

    @Test
    public void read_multipleBooleans() throws IOException {
        assertThat(
            sut.read(stream(" #true\n #false \t#true")),
            contains(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE));
    }

    @Test(expected = SyntaxError.class)
    public void read_badBoolean() throws IOException {
        sut.read(stream("#snafu"));
    }

    @Test
    public void read_oneSymbol() throws IOException {
        assertThat(sut.read(stream("snafu")), contains(new SlartiSymbol("snafu")));
    }

    @Test
    public void read_threeSymbol() throws IOException {
        assertThat(
            sut.read(stream("  foo  \n bar  \t baz  ")),
            contains(new SlartiSymbol("foo"), new SlartiSymbol("bar"), new SlartiSymbol("baz")));
    }

    @Test
    public void read_oneString() throws IOException {
        assertThat(sut.read(stream("\"foobar\"")), contains(new SlartiString("foobar")));
    }

    @Test
    public void read_oneStringWithWhitespaces() throws IOException {
        assertThat(sut.read(stream("\"  foobar\n\"")), contains(new SlartiString("  foobar\n")));
    }

    @Test
    public void read_threeStrings() throws IOException {
        assertThat(
            sut.read(stream("\"foo\" \"bar\" \"baz\"")),
            contains(new SlartiString("foo"), new SlartiString("bar"), new SlartiString("baz")));
    }

    @Test
    public void read_helloWorldQuote() throws IOException {
        assertThat(
            sut.read(stream("(println (quote hello-world!))")),
            contains(new SlartiList(Arrays.asList(
                new SlartiSymbol("println"), new QuoteSpecialForm(
                    new SlartiList(Arrays.asList(
                        new SlartiSymbol("hello-world!")
                    )))
            ))));
    }

    @Test
    public void read_helloWorldString() throws IOException {
        assertThat(
            sut.read(stream("(println \"Hello, World!\")")),
            contains(new SlartiList(Arrays.asList(
                new SlartiSymbol("println"),  new SlartiString("Hello, World!")
            ))));
    }

    @Test
    public void read_helloWorldStringWithComments() throws IOException {
        final List<SlartiNode> nodes = sut.read(stream(
            " ; One line comment with own line.\n" +
            "(println \"Hello, World!\") ; Comment at end of line\n"));

        assertThat(
            nodes,
            contains(new SlartiList(Arrays.asList(
                new SlartiSymbol("println"),  new SlartiString("Hello, World!")
            ))));
    }

    @Test
    public void read_ignoreComments() throws IOException {
        final String src = "(define n 10)\n" +
            "\n" +
            ";(if (> n 0) \n" +
            ";    (println \"true\") \n" +
            ";    (println \"false\"))\n" +
            "\n" +
            "(foo n)";

        final List<SlartiNode> nodes = sut.read(stream(src));

        assertThat(
            nodes,
            contains(
                new DefineSpecialForm(new SlartiList(new SlartiSymbol("n"), new SlartiNumber(10L))),
                new SlartiList(new SlartiSymbol("foo"), new SlartiSymbol("n"))
            ));
    }
}

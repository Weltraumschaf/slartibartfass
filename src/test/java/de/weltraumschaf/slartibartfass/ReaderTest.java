package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.special.QuoteSpecialForm;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

public class ReaderTest {
    private final Reader sut = new Reader();

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
    public void read_helloWorld() throws IOException {
        assertThat(
            sut.read(stream("(println (quote hello-world!))")),
            contains(new SlartiList(Arrays.asList(
                new SlartiSymbol("println"), new QuoteSpecialForm(
                    new SlartiList(Arrays.asList(
                        new SlartiSymbol("hello-world!")
                    )))
            ))));
    }
}
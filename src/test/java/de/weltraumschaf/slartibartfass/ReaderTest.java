package de.weltraumschaf.slartibartfass;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class ReaderTest {

    private Reader createSut(final String src) {
        return new Reader(new ByteArrayInputStream(src.getBytes()));
    }

    @Test
    public void empty() throws IOException {
        final Reader sut = createSut("");

        assertThat(sut.isEof(), is(true));
    }

    @Test
    public void notEmpty() throws IOException {
        final Reader sut = createSut("abc");

        assertThat(sut.isEof(), is(false));
        assertThat(sut.peek(), is('a'));
        assertThat(sut.next(), is('a'));
        assertThat(sut.isEof(), is(false));
        assertThat(sut.peek(), is('b'));
        assertThat(sut.next(), is('b'));
        assertThat(sut.isEof(), is(false));
        assertThat(sut.peek(), is('c'));
        assertThat(sut.next(), is('c'));
        assertThat(sut.isEof(), is(true));
    }
}

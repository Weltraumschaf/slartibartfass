package de.weltraumschaf.slartibartfass;

import org.junit.Test;
import org.mockito.internal.matchers.Null;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link ConsoleFormatter}.
 *
 * @author Sven Strittmatter
 */
public class ConsoleFormatterTest {
    private final ConsoleFormatter sut = new ConsoleFormatter();

    @Test(expected = NullPointerException.class)
    public void bold_inputMustNotBeNull() {
        sut.bold(null);
    }

    @Test
    public void bold_empty() {
        assertThat(sut.bold(""), is("\u001B[1m\u001B[0m"));
    }

    @Test
    public void bold() {
        assertThat(sut.bold("foo"), is("\u001B[1mfoo\u001B[0m"));
    }

    @Test(expected = NullPointerException.class)
    public void italic_inputMustNotBeNull() {
        sut.italic(null);
    }

    @Test
    public void italic_empty() {
        assertThat(sut.italic(""), is("\u001B[3m\u001B[0m"));
    }

    @Test
    public void italic() {
        assertThat(sut.italic("foo"), is("\u001B[3mfoo\u001B[0m"));
    }

    @Test(expected = NullPointerException.class)
    public void color_colorMustNotBeNul() {
        sut.color(null, "");
    }

    @Test(expected = NullPointerException.class)
    public void color_inputMustNotBeNull() {
        sut.color(ConsoleFormatter.Color.BLACK, null);
    }

    @Test
    public void color() {
        assertThat(sut.color(ConsoleFormatter.Color.CYAN, "foo"), is("\u001B[36mfoo\u001B[0m"));
    }
}

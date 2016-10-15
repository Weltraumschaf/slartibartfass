package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import org.hamcrest.Matchers;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

import static de.weltraumschaf.slartibartfass.node.Slarti.of;
import static de.weltraumschaf.slartibartfass.node.Slarti.sym;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Tests for {@link Environment}.
 *
 * @author Sven Strittmatter
 */
public class EnvironmentTest {
    @Test
    public void getValue_isPresent() {
        final Environment sut = new Environment();

        sut.putValue(sym("foo"), of("bar"));

        assertThat(sut.getValue(sym("foo")), Matchers.is(new MemoryBox(of("bar"))));
    }

    @Test(expected = RuntimeException.class)
    public void getValue_isNotPresent() {
        final Environment sut = new Environment();

        sut.getValue(sym("foo"));
    }

    @Test
    public void getValue_isPresentInParent() {
        final Environment parent = new Environment();
        parent.putValue(sym("foo"), of("bar"));
        final Environment sut = new Environment(parent);

        assertThat(sut.getValue(sym("foo")), is(new MemoryBox(of("bar"))));
    }

    @Test
    public void hasParent() {
        assertThat(new Environment().hasParent(), is(false));
        assertThat(new Environment(new Environment()).hasParent(), is(true));
    }

    @Test
    public void size() {
        final Environment sut = new Environment();

        assertThat(sut.size(), is(0));

        sut.putValue(sym("foo"), of(1L));

        assertThat(sut.size(), is(1));

        sut.putValue(sym("bar"), of(1L));
        sut.putValue(sym("baz"), of(1L));

        assertThat(sut.size(), is(3));
    }

    @Test
    public void print_withoutParent() {
        final Environment sut = new Environment();
        sut.putValue(sym("foo"), of(1L));
        sut.putValue(sym("bar"), of(2L));
        sut.putValue(sym("baz"), of(3L));
        final ByteArrayOutputStream output = new ByteArrayOutputStream();

        sut.print(new PrintStream(output));

        assertThat(new String(output.toByteArray(), StandardCharsets.UTF_8),
            is("  \u001B[1mbar     \u001B[0m -> 2\n"
                + "  \u001B[1mbaz     \u001B[0m -> 3\n"
                + "  \u001B[1mfoo     \u001B[0m -> 1\n"));
    }

    @Test
    public void print_withParent() {
        final Environment parent = new Environment();
        parent.putValue(sym("foo"), of(1L));
        final Environment sut = new Environment(parent);
        sut.putValue(sym("bar"), of(2L));
        sut.putValue(sym("baz"), of(3L));
        final ByteArrayOutputStream output = new ByteArrayOutputStream();

        sut.print(new PrintStream(output));

        assertThat(new String(output.toByteArray(), StandardCharsets.UTF_8),
            is("  \u001B[1mfoo     \u001B[0m -> 1\n"
                + "  \u001B[1mbar     \u001B[0m -> 2\n"
                + "  \u001B[1mbaz     \u001B[0m -> 3\n"));
    }

    @Test
    public void print_marksFunctionsAsBuiltInOrDefined() {
        final Environment sut = new Environment();
        final SlartiFunction builtIn = mock(SlartiFunction.class);
        when(builtIn.isBuiltIn()).thenReturn(true);
        sut.putValue(sym("foo"), builtIn);
        sut.putValue(sym("bar"), mock(SlartiFunction.class));
        final ByteArrayOutputStream output = new ByteArrayOutputStream();

        sut.print(new PrintStream(output));

        assertThat(new String(output.toByteArray(), StandardCharsets.UTF_8),
            is("  \u001B[1mbar     \u001B[0m -> defined fn\n"
                + "  \u001B[1mfoo     \u001B[0m -> \u001B[34mbuiltin fn\u001B[0m\n"));
    }
}

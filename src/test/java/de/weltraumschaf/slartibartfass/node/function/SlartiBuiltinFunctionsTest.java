package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

public class SlartiBuiltinFunctionsTest {
    private final IO io = mock(IO.class);
    private PrintStream outBackup;

    @Before
    public void setIo() {
        SlartiBuiltinFunctions.setIo(io);
    }

    @Test
    public void plus_name() {
        assertThat(SlartiBuiltinFunctions.PLUS.impl().name(), is("+"));
    }

    @Test
    public void plus_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.PLUS.impl().apply(Collections.emptyList());

        assertThat(result, is(0L));
    }

    @Test
    public void plus_oneArgs() {
        final Object result = SlartiBuiltinFunctions.PLUS.impl().apply(Arrays.asList(23L));

        assertThat(result, is(23L));
    }

    @Test
    public void plus_twoArgs() {
        final Object result = SlartiBuiltinFunctions.PLUS.impl().apply(Arrays.asList(23L, 42L));

        assertThat(result, is(65L));
    }

    @Test
    public void plus_threeArgs() {
        final Object result = SlartiBuiltinFunctions.PLUS.impl().apply(Arrays.asList(23L, 42L, 2L));

        assertThat(result, is(67L));
    }

    @Test
    public void minus_name() {
        assertThat(SlartiBuiltinFunctions.MINUS.impl().name(), is("-"));
    }

    @Test(expected = RuntimeException.class)
    public void minus_zeroArgs() {
        SlartiBuiltinFunctions.MINUS.impl().apply(Collections.emptyList());
    }

    @Test
    public void minus_oneArgs() {
        final Object result = SlartiBuiltinFunctions.MINUS.impl().apply(Arrays.asList(23L));

        assertThat(result, is(-23L));
    }

    @Test
    public void minus_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MINUS.impl().apply(Arrays.asList(23L, 42L));

        assertThat(result, is(-19L));
    }

    @Test
    public void minus_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MINUS.impl().apply(Arrays.asList(23L, 42L, 2L));

        assertThat(result, is(-21L));
    }

    @Test
    public void multiply_name() {
        assertThat(SlartiBuiltinFunctions.MULTIPLY.impl().name(), is("*"));
    }

    @Test
    public void multiply_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Collections.emptyList());

        assertThat(result, is(1L));
    }

    @Test
    public void multiply_oneArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Arrays.asList(23L));

        assertThat(result, is(23L));
    }

    @Test
    public void multiply_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Arrays.asList(10L, 23L));

        assertThat(result, is(230L));
    }

    @Test
    public void multiply_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Arrays.asList(10L, 23L, 2L));

        assertThat(result, is(460L));
    }

    @Test
    public void division_name() {
        assertThat(SlartiBuiltinFunctions.DIVISION.impl().name(), is("/"));
    }

    @Test(expected = RuntimeException.class)
    public void division_zeroArgs() {
        SlartiBuiltinFunctions.DIVISION.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void division_oneArgs() {
        SlartiBuiltinFunctions.DIVISION.impl().apply(Arrays.asList(23L));
    }

    @Test
    public void division_twoArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl().apply(Arrays.asList(55L, 10L));

        assertThat(result, is(5L));
    }

    @Test
    public void division_threeArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl().apply(Arrays.asList(55L, 10L, 2L));

        assertThat(result, is(2L));
    }

    @Test
    public void modulo_name() {
        assertThat(SlartiBuiltinFunctions.MODULO.impl().name(), is("%"));
    }

    @Test(expected = RuntimeException.class)
    public void modulo_zeroArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void modulo_oneArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(Arrays.asList());
    }

    @Test
    public void modulo_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl().apply(Arrays.asList(55L, 10L));

        assertThat(result, is(5L));
    }

    @Test
    public void modulo_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl().apply(Arrays.asList(55L, 10L, 2L));

        assertThat(result, is(1L));
    }

    @Test
    public void println_name() {
        assertThat(SlartiBuiltinFunctions.PRINTLN.impl().name(), is("println"));
    }

    @Test
    public void println_zeroArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Collections.emptyList());

        verify(io, times(1)).println("");
    }

    @Test
    public void println_oneArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Arrays.asList("foo"));

        verify(io, times(1)).println("foo");
    }

    @Test
    public void println_twoArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Arrays.asList("foo", "bar"));

        final InOrder inOrder = inOrder(io);
        inOrder.verify(io, times(1)).println("foobar");
    }

    @Test
    public void println_threeArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Arrays.asList("foo", "bar", "baz"));

        final InOrder inOrder = inOrder(io);
        inOrder.verify(io, times(1)).println("foobarbaz");
    }

    @Test
    public void and_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.emptyList()),
            is(Boolean.FALSE));
    }

    @Test
    public void and_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void and_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE)),
            is(Boolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.TRUE)),
            is(Boolean.TRUE));
    }

    @Test
    public void and_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)),
            is(Boolean.TRUE));
    }

    @Test
    public void or_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.emptyList()),
            is(Boolean.FALSE));
    }

    @Test
    public void or_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void or_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void or_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE)),
            is(Boolean.FALSE));
    }

}

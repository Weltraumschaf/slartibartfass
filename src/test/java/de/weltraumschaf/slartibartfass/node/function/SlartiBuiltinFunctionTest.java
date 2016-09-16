package de.weltraumschaf.slartibartfass.node.function;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

public class SlartiBuiltinFunctionTest {
    private final PrintStream out = mock(PrintStream.class);
    private PrintStream outBackup;

    @Before
    public void setOut() {
        outBackup = System.out;
        System.setOut(out);
    }

    @After
    public void restoreOut() {
        System.setOut(outBackup);
    }

    @Test
    public void plus_name() {
        assertThat(SlartiBuiltinFunction.PLUS.impl().name(), is("+"));
    }

    @Test
    public void plus_zeroArgs() {
        final Object result = SlartiBuiltinFunction.PLUS.impl().apply(Collections.emptyList());

        assertThat(result, is(0L));
    }

    @Test
    public void plus_oneArgs() {
        final Object result = SlartiBuiltinFunction.PLUS.impl().apply(Arrays.asList(23L));

        assertThat(result, is(23L));
    }

    @Test
    public void plus_twoArgs() {
        final Object result = SlartiBuiltinFunction.PLUS.impl().apply(Arrays.asList(23L, 42L));

        assertThat(result, is(65L));
    }

    @Test
    public void plus_threeArgs() {
        final Object result = SlartiBuiltinFunction.PLUS.impl().apply(Arrays.asList(23L, 42L, 2L));

        assertThat(result, is(67L));
    }

    @Test
    public void minus_name() {
        assertThat(SlartiBuiltinFunction.MINUS.impl().name(), is("-"));
    }

    @Test(expected = RuntimeException.class)
    public void minus_zeroArgs() {
        SlartiBuiltinFunction.MINUS.impl().apply(Collections.emptyList());
    }

    @Test
    public void minus_oneArgs() {
        final Object result = SlartiBuiltinFunction.MINUS.impl().apply(Arrays.asList(23L));

        assertThat(result, is(-23L));
    }

    @Test
    public void minus_twoArgs() {
        final Object result = SlartiBuiltinFunction.MINUS.impl().apply(Arrays.asList(23L, 42L));

        assertThat(result, is(-19L));
    }

    @Test
    public void minus_threeArgs() {
        final Object result = SlartiBuiltinFunction.MINUS.impl().apply(Arrays.asList(23L, 42L, 2L));

        assertThat(result, is(-21L));
    }

    @Test
    public void multiply_name() {
        assertThat(SlartiBuiltinFunction.MULTIPLY.impl().name(), is("*"));
    }

    @Test
    public void multiply_zeroArgs() {
        final Object result = SlartiBuiltinFunction.MULTIPLY.impl().apply(Collections.emptyList());

        assertThat(result, is(1L));
    }

    @Test
    public void multiply_oneArgs() {
        final Object result = SlartiBuiltinFunction.MULTIPLY.impl().apply(Arrays.asList(23L));

        assertThat(result, is(23L));
    }

    @Test
    public void multiply_twoArgs() {
        final Object result = SlartiBuiltinFunction.MULTIPLY.impl().apply(Arrays.asList(10L, 23L));

        assertThat(result, is(230L));
    }

    @Test
    public void multiply_threeArgs() {
        final Object result = SlartiBuiltinFunction.MULTIPLY.impl().apply(Arrays.asList(10L, 23L, 2L));

        assertThat(result, is(460L));
    }

    @Test
    public void division_name() {
        assertThat(SlartiBuiltinFunction.DIVISION.impl().name(), is("/"));
    }

    @Test(expected = RuntimeException.class)
    public void division_zeroArgs() {
        SlartiBuiltinFunction.DIVISION.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void division_oneArgs() {
        SlartiBuiltinFunction.DIVISION.impl().apply(Arrays.asList(23L));
    }

    @Test
    public void division_twoArgs() {
        final Object result = SlartiBuiltinFunction.DIVISION.impl().apply(Arrays.asList(55L, 10L));

        assertThat(result, is(5L));
    }

    @Test
    public void division_threeArgs() {
        final Object result = SlartiBuiltinFunction.DIVISION.impl().apply(Arrays.asList(55L, 10L, 2L));

        assertThat(result, is(2L));
    }

    @Test
    public void modulo_name() {
        assertThat(SlartiBuiltinFunction.MODULO.impl().name(), is("%"));
    }

    @Test(expected = RuntimeException.class)
    public void modulo_zeroArgs() {
        SlartiBuiltinFunction.MODULO.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void modulo_oneArgs() {
        SlartiBuiltinFunction.MODULO.impl().apply(Arrays.asList());
    }

    @Test
    public void modulo_twoArgs() {
        final Object result = SlartiBuiltinFunction.MODULO.impl().apply(Arrays.asList(55L, 10L));

        assertThat(result, is(5L));
    }

    @Test
    public void modulo_threeArgs() {
        final Object result = SlartiBuiltinFunction.MODULO.impl().apply(Arrays.asList(55L, 10L, 2L));

        assertThat(result, is(1L));
    }

    @Test
    public void println_name() {
        assertThat(SlartiBuiltinFunction.PRINTLN.impl().name(), is("println"));
    }

    @Test
    public void println_zeroArgs() {
        SlartiBuiltinFunction.PRINTLN.impl().apply(Collections.emptyList());

        verify(out, times(1)).println("");
    }

    @Test
    public void println_oneArgs() {
        SlartiBuiltinFunction.PRINTLN.impl().apply(Arrays.asList("foo"));

        verify(out, times(1)).println("foo");
    }

    @Test
    public void println_twoArgs() {
        SlartiBuiltinFunction.PRINTLN.impl().apply(Arrays.asList("foo", "bar"));

        final InOrder inOrder = inOrder(out);
        inOrder.verify(out, times(1)).println("foobar");
    }

    @Test
    public void println_threeArgs() {
        SlartiBuiltinFunction.PRINTLN.impl().apply(Arrays.asList("foo", "bar", "baz"));

        final InOrder inOrder = inOrder(out);
        inOrder.verify(out, times(1)).println("foobarbaz");
    }

    @Test
    public void and_zeroArgs() {
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Collections.emptyList()),
            is(Boolean.FALSE));
    }

    @Test
    public void and_oneArgs() {
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Collections.singletonList(Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Collections.singletonList(Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void and_twoArgs() {
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE)),
            is(Boolean.FALSE));
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.TRUE)),
            is(Boolean.TRUE));
    }

    @Test
    public void and_threeArgs() {
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.FALSE));
        assertThat(
            SlartiBuiltinFunction.AND.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)),
            is(Boolean.TRUE));
    }

    @Test
    public void or_zeroArgs() {
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Collections.emptyList()),
            is(Boolean.FALSE));
    }

    @Test
    public void or_oneArgs() {
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Collections.singletonList(Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Collections.singletonList(Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void or_twoArgs() {
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE)),
            is(Boolean.FALSE));
    }

    @Test
    public void or_threeArgs() {
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE)),
            is(Boolean.TRUE));
        assertThat(
            SlartiBuiltinFunction.OR.impl().apply(Arrays.asList(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE)),
            is(Boolean.FALSE));
    }

}

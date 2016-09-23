package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiString;
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

    @Before
    public void setIo() {
        SlartiBuiltinFunctions.register(new Environment(), io);
    }

    @Test
    public void plus_name() {
        assertThat(SlartiBuiltinFunctions.ADD.impl().name(), is("+"));
    }

    @Test
    public void plus_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl().apply(Collections.emptyList());

        assertThat(result, is(new SlartiInteger(0L)));
    }

    @Test
    public void plus_oneArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl().
            apply(Collections.singletonList(new SlartiInteger(23L)));

        assertThat(result, is(new SlartiInteger(23L)));
    }

    @Test
    public void plus_twoArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L)));

        assertThat(result, is(new SlartiInteger(65L)));
    }

    @Test
    public void plus_threeArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(67L)));
    }

    @Test
    public void minus_name() {
        assertThat(SlartiBuiltinFunctions.SUBTRACT.impl().name(), is("-"));
    }

    @Test(expected = RuntimeException.class)
    public void minus_zeroArgs() {
        SlartiBuiltinFunctions.SUBTRACT.impl().apply(Collections.emptyList());
    }

    @Test
    public void minus_oneArgs() {
        final Object result = SlartiBuiltinFunctions.SUBTRACT.impl()
            .apply(Collections.singletonList(new SlartiInteger(23L)));

        assertThat(result, is(new SlartiInteger(-23L)));
    }

    @Test
    public void minus_twoArgs() {
        final Object result = SlartiBuiltinFunctions.SUBTRACT.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L)));

        assertThat(result, is(new SlartiInteger(-19L)));
    }

    @Test
    public void minus_threeArgs() {
        final Object result = SlartiBuiltinFunctions.SUBTRACT.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(-21L)));
    }

    @Test
    public void multiply_name() {
        assertThat(SlartiBuiltinFunctions.MULTIPLY.impl().name(), is("*"));
    }

    @Test
    public void multiply_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Collections.emptyList());

        assertThat(result, is(new SlartiInteger(1L)));
    }

    @Test
    public void multiply_oneArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl()
            .apply(Collections.singletonList(new SlartiInteger(23L)));

        assertThat(result, is(new SlartiInteger(23L)));
    }

    @Test
    public void multiply_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl()
            .apply(Arrays.asList(new SlartiInteger(10L), new SlartiInteger(23L)));

        assertThat(result, is(new SlartiInteger(230L)));
    }

    @Test
    public void multiply_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl()
            .apply(Arrays.asList(new SlartiInteger(10L), new SlartiInteger(23L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(460L)));
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
        SlartiBuiltinFunctions.DIVISION.impl().apply(Collections.singletonList(new SlartiInteger(23L)));
    }

    @Test
    public void division_twoArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L)));

        assertThat(result, is(new SlartiInteger(5L)));
    }

    @Test
    public void division_threeArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(2L)));
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
        SlartiBuiltinFunctions.MODULO.impl().apply(Collections.singletonList(new SlartiInteger(23L)));
    }

    @Test
    public void modulo_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L)));

        assertThat(result, is(new SlartiInteger(5L)));
    }

    @Test
    public void modulo_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(1L)));
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
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Collections.singletonList(new SlartiString("foo")));

        verify(io, times(1)).println("foo");
    }

    @Test
    public void println_twoArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(Arrays.asList(new SlartiString("foo"), new SlartiInteger(23L)));

        final InOrder inOrder = inOrder(io);
        inOrder.verify(io, times(1)).println("foo23");
    }

    @Test
    public void println_threeArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl()
            .apply(Arrays.asList(new SlartiString("foo"), new SlartiString("bar"), new SlartiString("baz")));

        final InOrder inOrder = inOrder(io);
        inOrder.verify(io, times(1)).println("foobarbaz");
    }

    @Test
    public void and_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.emptyList()),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void and_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void and_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
    }

    @Test
    public void and_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().
                apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl()
                .apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.TRUE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
    }

    @Test
    public void or_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.emptyList()),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

}

package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

public class SlartiBuiltinFunctionsTest {
    private final IO io = mock(IO.class);

    @Before
    public void setIo() {
        SlartiBuiltinFunctions.register(new Environment(), io);
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
        SlartiBuiltinFunctions.PRINTLN.impl().apply(of("foo"));

        verify(io, times(1)).println("foo");
    }

    @Test
    public void println_twoArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(of("foo"), of(23L));

        verify(io, times(1)).println("foo23");
    }

    @Test
    public void println_threeArgs() {
        SlartiBuiltinFunctions.PRINTLN.impl().apply(of("foo"), of("bar"), of("baz"));

        verify(io, times(1)).println("foobarbaz");
    }

    @Test
    public void print_name() {
        assertThat(SlartiBuiltinFunctions.PRINT.impl().name(), is("print"));
    }

    @Test
    public void print_zeroArgs() {
        SlartiBuiltinFunctions.PRINT.impl().apply(Collections.emptyList());

        verify(io, times(1)).print("");
    }

    @Test
    public void print_oneArgs() {
        SlartiBuiltinFunctions.PRINT.impl().apply(of("foo"));

        verify(io, times(1)).print("foo");
    }

    @Test
    public void print_twoArgs() {
        SlartiBuiltinFunctions.PRINT.impl().apply(of("foo"), of(23L));

        verify(io, times(1)).print("foo23");
    }

    @Test
    public void print_threeArgs() {
        SlartiBuiltinFunctions.PRINT.impl().apply(of("foo"), of("bar"), of("baz"));

        verify(io, times(1)).print("foobarbaz");
    }

    @Test
    public void list_name() {
        assertThat(SlartiBuiltinFunctions.LIST.impl().name(), is("list"));
    }

    @Test
    public void list_empty() {
        assertThat(SlartiBuiltinFunctions.LIST.impl().apply(Collections.emptyList()), is(SlartiList.EMPTY));
    }

    @Test
    public void list_some() {
        assertThat(
            SlartiBuiltinFunctions.LIST.impl().apply(of("foo"), of("bar"), of("baz")),
            is(list(of("foo"), of("bar"), of("baz"))));
    }

    @Test
    public void head_name() {
        assertThat(SlartiBuiltinFunctions.HEAD.impl().name(), is("head"));
    }

    @Test
    public void head_empty() {
        assertThat(SlartiBuiltinFunctions.HEAD.impl().apply(SlartiList.EMPTY), is(SlartiBoolean.FALSE));
    }

    @Test
    public void head_some() {
        assertThat(
            SlartiBuiltinFunctions.HEAD.impl().apply(list(of("foo"), of("bar"), of("baz"))),
            is(of("foo")));
    }

    @Test
    public void tail_name() {
        assertThat(SlartiBuiltinFunctions.TAIL.impl().name(), is("tail"));
    }

    @Test
    public void tail_empty() {
        assertThat(SlartiBuiltinFunctions.TAIL.impl().apply(SlartiList.EMPTY), is(SlartiList.EMPTY));
    }

    @Test
    public void tail_some() {
        assertThat(
            SlartiBuiltinFunctions.TAIL.impl().apply(list(of("foo"), of("bar"), of("baz"))),
            is(list(of("bar"), of("baz"))));
    }
}

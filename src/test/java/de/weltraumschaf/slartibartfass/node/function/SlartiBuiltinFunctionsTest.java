package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiString;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;

import java.util.Arrays;
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

}

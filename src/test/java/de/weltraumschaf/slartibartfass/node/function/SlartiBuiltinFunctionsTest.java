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

}

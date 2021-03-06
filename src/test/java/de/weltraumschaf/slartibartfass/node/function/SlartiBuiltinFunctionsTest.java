package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.backend.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiBuiltinFunctions}.
 *
 * @author Sven Strittmatter
 */
public class SlartiBuiltinFunctionsTest {
    private final IO io = mock(IO.class);

    @Before
    public void setIo() {
        SlartiBuiltinFunctions.register(new Environment(), io);
    }

    @Test
    public void println_name() {
        assertThat(SlartiBuiltinFunctions.PRINTLN.impl().symbol(), is(sym("println")));
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
        assertThat(SlartiBuiltinFunctions.PRINT.impl().symbol(), is(sym("print")));
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
        assertThat(SlartiBuiltinFunctions.LIST.impl().symbol(), is(sym("list")));
    }

    @Test
    public void list_empty() {
        assertThat(SlartiBuiltinFunctions.LIST.impl().apply(Collections.emptyList()), is(SlartiList.NIL));
    }

    @Test
    public void list_some() {
        assertThat(
            SlartiBuiltinFunctions.LIST.impl().apply(of("foo"), of("bar"), of("baz")),
            is(list(of("foo"), of("bar"), of("baz"))));
    }

    @Test
    public void head_name() {
        assertThat(SlartiBuiltinFunctions.HEAD.impl().symbol(), is(sym("head")));
    }

    @Test
    public void head_empty() {
        assertThat(SlartiBuiltinFunctions.HEAD.impl().apply(SlartiList.NIL), is(SlartiBoolean.FALSE));
    }

    @Test
    public void head_some() {
        assertThat(
            SlartiBuiltinFunctions.HEAD.impl().apply(list(of("foo"), of("bar"), of("baz"))),
            is(of("foo")));
    }

    @Test
    public void tail_name() {
        assertThat(SlartiBuiltinFunctions.TAIL.impl().symbol(), is(sym("tail")));
    }

    @Test
    public void tail_empty() {
        assertThat(SlartiBuiltinFunctions.TAIL.impl().apply(SlartiList.NIL), is(SlartiList.NIL));
    }

    @Test
    public void tail_some() {
        assertThat(
            SlartiBuiltinFunctions.TAIL.impl().apply(list(of("foo"), of("bar"), of("baz"))),
            is(list(of("bar"), of("baz"))));
    }
}

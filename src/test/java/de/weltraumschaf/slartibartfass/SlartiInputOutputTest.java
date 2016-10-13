package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import org.junit.Test;

import java.io.InputStream;
import java.io.PrintStream;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;

/**
 * Tests for {@link SlartiInputOutput}.
 *
 * @author Sven Strittmatter
 */
public class SlartiInputOutputTest {

    private final IO io = mock(IO.class);
    private final SlartiInputOutput sut = new SlartiInputOutput(io, true);

    @Test
    public void debug() {
        sut.debug("foo %d bar", 42);

        verify(io, times(1)).println("\u001B[34m[D] foo 42 bar\u001B[0m");
    }

    @Test
    public void debug_disabled() {
        new SlartiInputOutput(io, false).debug("foo");

        verify(io, never()).println(anyString());
    }

    @Test
    public void error() {
        sut.error("foo %d bar", 42);

        verify(io, times(1)).errorln("\u001B[31m\u001B[1m[E] foo 42 bar\u001B[0m");
    }

    @Test
    public void fatal() {
        sut.fatal("foo %d bar", 42);

        verify(io, times(1)).errorln("\u001B[31m\u001B[1m[F] foo 42 bar\u001B[0m");
    }

    @Test
    public void printStackTraceOnDebug() {
        final Throwable e = new Throwable();

        sut.printStackTraceOnDebug(e);

        verify(io, timeout(1)).printStackTrace(e);
    }

    @Test
    public void printStackTraceOnDebug_doesNothingIfDebugIsNotEnabled() {
        new SlartiInputOutput(io, false).printStackTraceOnDebug(new Throwable());

        verify(io, never()).printStackTrace(any(Throwable.class));
    }

    @Test
    public void print() {
        sut.print("foo");

        verify(io, times(1)).print("foo");
    }

    @Test
    public void println() {
        sut.println("foo");

        verify(io, timeout(1)).println("foo");
    }

    @Test
    public void getStdout() {
        final PrintStream stream = mock(PrintStream.class);
        when(io.getStdout()).thenReturn(stream);

        assertThat(sut.getStdout(), is(sameInstance(stream)));
    }

    @Test
    public void getStdin() {
        final InputStream stream = mock(InputStream.class);
        when(io.getStdin()).thenReturn(stream);

        assertThat(sut.getStdin(), is(sameInstance(stream)));
    }

    @Test
    public void getIo() {
        assertThat(sut.getIo(), is(sameInstance(io)));
    }
}

package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import org.junit.Test;

import static org.mockito.Mockito.*;

/**
 * Tests for {@link SlartInputOutput}.
 *
 * @author Sven Strittmatter
 */
public class SlartInputOutputTest {

    private final IO io = mock(IO.class);
    private final SlartInputOutput sut = new SlartInputOutput(io, true);

    @Test
    public void debug() {
        sut.debug("foo %d bar", 42);

        verify(io, times(1)).println("\u001B[34m[D] foo 42 bar\u001B[0m");
    }

    @Test
    public void debug_disabled() {
        new SlartInputOutput(io, false).debug("foo");

        verify(io, never()).println(anyString());
    }

    @Test
    public void error() {
        sut.error("foo %d bar", 42);

        verify(io, times(1)).errorln("\u001B[31m[E] foo 42 bar\u001B[0m");
    }

    @Test
    public void fatal() {
        sut.fatal("foo %d bar", 42);

        verify(io, times(1)).errorln("\u001B[31m[F] foo 42 bar\u001B[0m");
    }
}

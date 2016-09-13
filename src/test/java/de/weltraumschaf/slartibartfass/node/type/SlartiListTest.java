package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class SlartiListTest {

    private final SlartiNode a = mock(SlartiNode.class);
    private final SlartiNode b = mock(SlartiNode.class);
    private final SlartiNode c = mock(SlartiNode.class);
    private final SlartiList sut = new SlartiList(Arrays.asList(a, b, c));

    @Before
    public void mockToString() {
        when(a.toString()).thenReturn("a");
        when(b.toString()).thenReturn("b");
        when(c.toString()).thenReturn("c");
    }

    @Test
    @Ignore
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiList.class).suppress(Warning.STRICT_INHERITANCE).verify();
    }

    @Test
    public void head() throws Exception {
        assertThat(sut.head(), is(sameInstance(a)));
    }

    @Test
    public void tail() throws Exception {
        assertThat(sut.tail(), is(new SlartiList(Arrays.asList(b, c))));
    }

    @Test
    public void eval() {
        final Environment env = new Environment();
        final SlartiFunction fn = mock(SlartiFunction.class);
        when(a.eval(env)).thenReturn(fn);
        when(b.eval(env)).thenReturn("foo");
        when(c.eval(env)).thenReturn("bar");

        sut.eval(env);

        verify(fn, times(1)).apply(Arrays.asList("foo", "bar"));
    }

    @Test
    public void iterator() {
        assertThat(sut, contains(a, b, c));
    }

    @Test
    public void string() {
        assertThat(sut.toString(), is("(a, b, c)"));
    }
}
package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiSymbolTest {

    private final SlartiSymbol sut = new SlartiSymbol("foo");

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiSymbol.class).verify();
    }

    @Test
    public void string() {
        assertThat(sut.toString(), is("foo"));
    }

    @Test
    public void eval() {
        final Environment env = new Environment();
        env.putValue("foo", "bar");

        assertThat(sut.eval(env), is("bar"));
    }
}

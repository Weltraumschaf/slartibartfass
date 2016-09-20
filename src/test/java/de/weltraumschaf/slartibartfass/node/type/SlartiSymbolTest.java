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

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(false));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(false));
        assertThat(sut.isOf(SlartiString.class), is(false));
        assertThat(sut.isOf(SlartiSymbol.class), is(true));
    }
}

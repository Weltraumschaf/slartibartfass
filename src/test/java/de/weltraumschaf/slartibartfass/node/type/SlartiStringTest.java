package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiStringTest {

    private final SlartiString sut = new SlartiString("foo");

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiString.class).verify();
    }

    @Test
    public void string() {
        assertThat(sut.toString(), is("\"foo\""));
    }

    @Test
    public void eval() {
        assertThat(sut.eval(new Environment()), is("foo"));
    }

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(false));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(false));
        assertThat(sut.isOf(SlartiString.class), is(true));
        assertThat(sut.isOf(SlartiSymbol.class), is(false));
    }
}

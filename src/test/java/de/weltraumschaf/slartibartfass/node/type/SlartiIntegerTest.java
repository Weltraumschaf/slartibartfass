package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.*;

public class SlartiIntegerTest {

    private final SlartiInteger sut = new SlartiInteger(42L);

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiInteger.class).verify();
    }

    @Test
    public void eval() {
        assertThat(sut.eval(new Environment()), is(42L));
    }

    @Test
    public void string() {
        assertThat(sut.toString(), is("42"));
    }

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(true));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(false));
        assertThat(sut.isOf(SlartiString.class), is(false));
        assertThat(sut.isOf(SlartiSymbol.class), is(false));
    }
}
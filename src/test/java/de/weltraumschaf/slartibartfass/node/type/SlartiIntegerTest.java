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

        assertThat(sut.isBoolean(), is(false));
        assertThat(sut.isInteger(), is(true));
        assertThat(sut.isList(), is(false));
        assertThat(sut.isReal(), is(false));
        assertThat(sut.isString(), is(false));
        assertThat(sut.isSymbol(), is(false));
    }

    @Test
    public void value() {
        assertThat(sut.value(), is(42L));
    }

    @Test
    public void castToBoolean() {
        assertThat(sut.castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(new SlartiInteger(-3L).castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(new SlartiInteger(0L).castToBoolean(), is(SlartiBoolean.FALSE));
    }

    @Test
    public void castToInteger() {
        assertThat(sut.castToInteger(), is(new SlartiInteger(42L)));
    }

    @Test
    public void castToReal() {
        assertThat(sut.castToReal(), is(new SlartiReal(42d)));
    }

    @Test
    public void castToString() {
        assertThat(sut.castToString(), is(new SlartiString("42")));
    }

    @Test
    public void castToList() {
        assertThat(sut.castToList(), is(new SlartiList(sut)));
    }
}
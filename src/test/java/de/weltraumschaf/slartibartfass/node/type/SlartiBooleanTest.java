package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.backend.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.*;

public class SlartiBooleanTest {

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiBoolean.class).verify();
    }

    @Test
    public void eval() {
        assertThat(SlartiBoolean.FALSE.eval(new Environment()), is(SlartiBoolean.FALSE));
        assertThat(SlartiBoolean.TRUE.eval(new Environment()), is(SlartiBoolean.TRUE));
    }

    @Test
    public void string() {
        assertThat(SlartiBoolean.FALSE.toString(), is("#false"));
        assertThat(SlartiBoolean.TRUE.toString(), is("#true"));
    }

    @Test
    public void isOf() {
        assertThat(SlartiBoolean.TRUE.isOf(SlartiBoolean.class), is(true));
        assertThat(SlartiBoolean.TRUE.isOf(SlartiInteger.class), is(false));
        assertThat(SlartiBoolean.TRUE.isOf(SlartiList.class), is(false));
        assertThat(SlartiBoolean.TRUE.isOf(SlartiReal.class), is(false));
        assertThat(SlartiBoolean.TRUE.isOf(SlartiString.class), is(false));
        assertThat(SlartiBoolean.TRUE.isOf(SlartiSymbol.class), is(false));

        assertThat(SlartiBoolean.FALSE.isBoolean(), is(true));
        assertThat(SlartiBoolean.FALSE.isInteger(), is(false));
        assertThat(SlartiBoolean.FALSE.isList(), is(false));
        assertThat(SlartiBoolean.FALSE.isReal(), is(false));
        assertThat(SlartiBoolean.FALSE.isString(), is(false));
        assertThat(SlartiBoolean.FALSE.isSymbol(), is(false));
    }

    @Test
    public void value() {
        assertThat(SlartiBoolean.TRUE.value(), is(Boolean.TRUE));
        assertThat(SlartiBoolean.FALSE.value(), is(Boolean.FALSE));
    }

    @Test
    public void castToBoolean() {
        assertThat(SlartiBoolean.TRUE.castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(SlartiBoolean.FALSE.castToBoolean(), is(SlartiBoolean.FALSE));
    }

    @Test
    public void castToInteger() {
        assertThat(SlartiBoolean.TRUE.castToInteger(), is(new SlartiInteger(1L)));
        assertThat(SlartiBoolean.FALSE.castToInteger(), is(new SlartiInteger(0L)));
    }

    @Test
    public void castToReal() {
        assertThat(SlartiBoolean.TRUE.castToReal(), is(new SlartiReal(1d)));
        assertThat(SlartiBoolean.FALSE.castToReal(), is(new SlartiReal(0d)));
    }

    @Test
    public void castToString() {
        assertThat(SlartiBoolean.TRUE.castToString(), is(new SlartiString("#true")));
        assertThat(SlartiBoolean.FALSE.castToString(), is(new SlartiString("#false")));
    }

    @Test
    public void castToList() {
        assertThat(SlartiBoolean.TRUE.castToList(), is(new SlartiList(SlartiBoolean.TRUE)));
    }
}
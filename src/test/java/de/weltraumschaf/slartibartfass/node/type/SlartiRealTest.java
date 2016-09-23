package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiRealTest {
    private final SlartiReal sut = new SlartiReal(3.14d);

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiReal.class).verify();
    }

    @Test
    public void eval() {
        assertThat(sut.eval(new Environment()), is(new SlartiReal(3.14d)));
    }

    @Test
    public void string() {
        assertThat(sut.toString(), is("3.14"));
    }

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(false));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(true));
        assertThat(sut.isOf(SlartiString.class), is(false));
        assertThat(sut.isOf(SlartiSymbol.class), is(false));

        assertThat(sut.isBoolean(), is(false));
        assertThat(sut.isInteger(), is(false));
        assertThat(sut.isList(), is(false));
        assertThat(sut.isReal(), is(true));
        assertThat(sut.isString(), is(false));
        assertThat(sut.isSymbol(), is(false));
    }

    @Test
    public void value() {
        assertThat(sut.value(), is(3.14d));
    }

    @Test
    public void castToBoolean() {
        assertThat(sut.castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(new SlartiReal(-3.5d).castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(new SlartiReal(0d).castToBoolean(), is(SlartiBoolean.FALSE));
    }

    @Test
    public void castToInteger() {
        assertThat(sut.castToInteger(), is(new SlartiInteger(3L)));
    }

    @Test
    public void castToReal() {
        assertThat(sut.castToReal(), is(new SlartiReal(3.14d)));
    }

    @Test
    public void castToString() {
        assertThat(sut.castToString(), is(new SlartiString("3.14")));
    }

    @Test
    public void castToList() {
        assertThat(sut.castToList(), is(new SlartiList(sut)));
    }
}

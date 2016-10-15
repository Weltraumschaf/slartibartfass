package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.backend.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link SlartiString}.
 *
 * @author Sven Strittmatter
 */
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
        assertThat(sut.eval(new Environment()), is(new SlartiString("foo")));
    }

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(false));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(false));
        assertThat(sut.isOf(SlartiString.class), is(true));
        assertThat(sut.isOf(SlartiSymbol.class), is(false));

        assertThat(sut.isBoolean(), is(false));
        assertThat(sut.isInteger(), is(false));
        assertThat(sut.isList(), is(false));
        assertThat(sut.isReal(), is(false));
        assertThat(sut.isString(), is(true));
        assertThat(sut.isSymbol(), is(false));
    }

    @Test
    public void value() {
        assertThat(sut.value(), is("foo"));
    }

    @Test
    public void castToBoolean() {
        assertThat(sut.castToBoolean(), is(SlartiBoolean.FALSE));
        assertThat(new SlartiString("#true").castToBoolean(), is(SlartiBoolean.TRUE));
        assertThat(new SlartiString("#false").castToBoolean(), is(SlartiBoolean.FALSE));
    }

    @Test
    public void castToInteger() {
        assertThat(sut.castToInteger(), is(new SlartiInteger(0L)));
        assertThat(new SlartiString("42").castToInteger(), is(new SlartiInteger(42L)));
        assertThat(new SlartiString("-3.14").castToInteger(), is(new SlartiInteger(-3L)));
    }

    @Test
    public void castToReal() {
        assertThat(sut.castToReal(), is(new SlartiReal(0d)));
        assertThat(new SlartiString("42").castToReal(), is(new SlartiReal(42d)));
        assertThat(new SlartiString("-3.14").castToReal(), is(new SlartiReal(-3.14d)));
    }

    @Test
    public void castToString() {
        assertThat(sut.castToString(), is(new SlartiString("foo")));
    }

    @Test
    public void castToList() {
        assertThat(sut.castToList(), is(new SlartiList(sut)));
    }
}

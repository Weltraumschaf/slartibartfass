package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link SlartiSymbol}.
 *
 * @author Sven Strittmatter
 */
public class SlartiSymbolTest {

    @Rule
    public final ExpectedException thrown = ExpectedException.none();
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
        env.putValue("foo", new SlartiString("bar"));

        assertThat(sut.eval(env), is(new SlartiString("bar")));
    }

    @Test
    public void isOf() {
        assertThat(sut.isOf(SlartiBoolean.class), is(false));
        assertThat(sut.isOf(SlartiInteger.class), is(false));
        assertThat(sut.isOf(SlartiList.class), is(false));
        assertThat(sut.isOf(SlartiReal.class), is(false));
        assertThat(sut.isOf(SlartiString.class), is(false));
        assertThat(sut.isOf(SlartiSymbol.class), is(true));

        assertThat(sut.isBoolean(), is(false));
        assertThat(sut.isInteger(), is(false));
        assertThat(sut.isList(), is(false));
        assertThat(sut.isReal(), is(false));
        assertThat(sut.isString(), is(false));
        assertThat(sut.isSymbol(), is(true));
    }

    @Test
    public void value() {
        assertThat(sut.value(), is("foo"));
    }

    @Test
    public void castToBoolean() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage("SlartiSymbol does not support cast to SlartiBoolean!");

        sut.castToBoolean();
    }

    @Test
    public void castToInteger() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage("SlartiSymbol does not support cast to SlartiInteger!");

        sut.castToInteger();
    }

    @Test
    public void castToReal() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage("SlartiSymbol does not support cast to SlartiReal!");

        sut.castToReal();
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

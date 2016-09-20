package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
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
        assertThat(SlartiBoolean.FALSE.eval(new Environment()), is(Boolean.FALSE));
        assertThat(SlartiBoolean.TRUE.eval(new Environment()), is(Boolean.TRUE));
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
    }
}
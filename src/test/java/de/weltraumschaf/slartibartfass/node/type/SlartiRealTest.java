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
        assertThat(sut.eval(new Environment()), is(3.14d));
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
    }
}

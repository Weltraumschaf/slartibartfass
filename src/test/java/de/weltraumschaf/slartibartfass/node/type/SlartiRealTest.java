package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiRealTest {
    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiReal.class).verify();
    }

    @Test
    public void eval() {
        assertThat(new SlartiReal(3.14d).eval(new Environment()), is(3.14d));
    }

    @Test
    public void string() {
        assertThat(new SlartiReal(3.14d).toString(), is("3.14"));
    }
}

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
}

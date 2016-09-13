package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.*;

public class SlartiNumberTest {

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiNumber.class).verify();
    }

    @Test
    public void eval() {
        assertThat(new SlartiNumber(42L).eval(new Environment()), is(42L));
    }

    @Test
    public void string() {
        assertThat(new SlartiNumber(42L).toString(), is("42"));
    }
}
package de.weltraumschaf.slartibartfass;

import org.junit.Test;

import static de.weltraumschaf.slartibartfass.node.Slarti.of;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link Environment}.
 */
public class EnvironmentTest {
    @Test
    public void getValue_isPresent() {
        final Environment sut = new Environment();

        sut.putValue("foo", of("bar"));

        assertThat(sut.getValue("foo"), is(of("bar")));
    }

    @Test(expected = RuntimeException.class)
    public void getValue_isNotPresent() {
        final Environment sut = new Environment();

        sut.getValue("foo");
    }

    @Test
    public void getValue_isPresentInParent() {
        final Environment parent = new Environment();
        parent.putValue("foo", of("bar"));
        final Environment sut = new Environment(parent);

        assertThat(sut.getValue("foo"), is(of("bar")));
    }
}

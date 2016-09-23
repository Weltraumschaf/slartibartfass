package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.type.SlartiString;
import org.junit.Test;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class EnvironmentTest {
    @Test
    public void getValue_isPresent() {
        final Environment sut = new Environment();

        sut.putValue("foo", new SlartiString("bar"));

        assertThat(sut.getValue("foo"), is(new SlartiString("bar")));
    }

    @Test(expected = RuntimeException.class)
    public void getValue_isNotPresent() {
        final Environment sut = new Environment();

        sut.getValue("foo");
    }

    @Test
    public void getValue_isPresentInParent() {
        final Environment parent = new Environment();
        parent.putValue("foo", new SlartiString("bar"));
        final Environment sut = new Environment(parent);

        assertThat(sut.getValue("foo"), is(new SlartiString("bar")));
    }
}

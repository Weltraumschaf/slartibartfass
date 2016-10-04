package de.weltraumschaf.slartibartfass;

import org.junit.Test;

import static de.weltraumschaf.slartibartfass.node.Slarti.of;
import static de.weltraumschaf.slartibartfass.node.Slarti.sym;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link Environment}.
 *
 * @author Sven Strittmatter
 */
public class EnvironmentTest {
    @Test
    public void getValue_isPresent() {
        final Environment sut = new Environment();

        sut.putValue(sym("foo"), of("bar"));

        assertThat(sut.getValue(sym("foo")), is(new MemoryBox(sym("foo"), of("bar"))));
    }

    @Test(expected = RuntimeException.class)
    public void getValue_isNotPresent() {
        final Environment sut = new Environment();

        sut.getValue(sym("foo"));
    }

    @Test
    public void getValue_isPresentInParent() {
        final Environment parent = new Environment();
        parent.putValue(sym("foo"), of("bar"));
        final Environment sut = new Environment(parent);

        assertThat(sut.getValue(sym("foo")), is(new MemoryBox(sym("foo"), of("bar"))));
    }
}

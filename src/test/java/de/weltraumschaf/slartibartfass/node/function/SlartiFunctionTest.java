package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link SlartiFunction}.
 *
 * @author Sven Strittmatter
 */
public class SlartiFunctionTest {

    private final SlartiFunction sut = new SlartiFunction("symbol") {
        @Override
        public SlartiNode apply(List<SlartiNode> args) {
            return null;
        }

        @Override
        public boolean isBuiltIn() {
            return false;
        }
    };

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiFunction.class).verify();
    }

    @Test
    public void eval() {
        assertThat(sut.eval(new Environment()), is(sameInstance(sut)));
    }

}

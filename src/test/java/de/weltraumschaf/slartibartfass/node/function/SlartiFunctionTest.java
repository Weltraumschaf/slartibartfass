package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.Environment;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

public class SlartiFunctionTest {

    private final SlartiFunction sut = new SlartiFunction("name") {
        @Override
        public Object apply(List<Object> args) {
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

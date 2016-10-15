package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.backend.Environment;
import de.weltraumschaf.slartibartfass.backend.MemoryBox;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import org.junit.Test;

import java.util.Arrays;

import static de.weltraumschaf.slartibartfass.node.Slarti.of;
import static de.weltraumschaf.slartibartfass.node.Slarti.sym;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link DefineSpecialForm}.
 *
 * @author Sven Strittmatter
 */
public class DefineSpecialFormTest {
    @Test
    public void eval() {
        final Environment env = new Environment();
        final SlartiSpecialForm sut = new DefineSpecialForm(new SlartiList(
            Arrays.asList(sym("foo"), of(42L))
        ));

        assertThat(sut.eval(env), is(sym("foo")));

        assertThat(env.getValue(sym("foo")), is(new MemoryBox(of(42L))));
    }
}

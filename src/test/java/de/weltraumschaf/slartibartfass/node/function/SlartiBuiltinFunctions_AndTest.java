package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiBuiltinFunctions#AND}.
 *
 * @author Sven Strittmatter
 */
public class SlartiBuiltinFunctions_AndTest {
    @Test
    public void and_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.emptyList()),
            is(of(false)));
    }

    @Test
    public void and_oneArgs() {
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(true)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(false)), is(of(false)));
    }

    @Test
    public void and_twoArgs() {
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(true), of(false)), is(of(false)));
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(true), of(true)), is(of(true)));
    }

    @Test
    public void and_threeArgs() {
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(true), of(false), of(true)), is(of(false)));
        assertThat(SlartiBuiltinFunctions.AND.impl().apply(of(true), of(true), of(true)), is(of(true)));
    }
}

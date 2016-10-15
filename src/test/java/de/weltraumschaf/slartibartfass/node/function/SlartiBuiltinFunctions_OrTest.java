package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiBuiltinFunctions#OR}.
 *
 * @author Sven Strittmatter
 */
public class SlartiBuiltinFunctions_OrTest {
    @Test
    public void or_zeroArgs() {
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(Collections.emptyList()), is(of(false)));
    }

    @Test
    public void or_oneArgs() {
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(true)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(false)), is(of(false)));
    }

    @Test
    public void or_twoArgs() {
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(true), of(false)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(false), of(true)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(false), of(false)), is(of(false)));
    }

    @Test
    public void or_threeArgs() {
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(true), of(false), of(true)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(false), of(false), of(true)), is(of(true)));
        assertThat(SlartiBuiltinFunctions.OR.impl().apply(of(false), of(false), of(false)), is(of(false)));
    }
}

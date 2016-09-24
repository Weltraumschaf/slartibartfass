package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiBuiltinFunctions_AndTest {
    @Test
    public void and_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.emptyList()),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void and_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Collections.singletonList(SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void and_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
    }

    @Test
    public void and_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.AND.impl().
                apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.FALSE));
        assertThat(
            SlartiBuiltinFunctions.AND.impl()
                .apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.TRUE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
    }
}

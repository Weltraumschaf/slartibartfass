package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiBuiltinFunctions_OrTest {
    @Test
    public void or_zeroArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.emptyList()),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_oneArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Collections.singletonList(SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_twoArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl().apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }

    @Test
    public void or_threeArgs() {
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.TRUE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE, SlartiBoolean.TRUE)),
            is(SlartiBoolean.TRUE));
        assertThat(
            SlartiBuiltinFunctions.OR.impl()
                .apply(Arrays.asList(SlartiBoolean.FALSE, SlartiBoolean.FALSE, SlartiBoolean.FALSE)),
            is(SlartiBoolean.FALSE));
    }
}

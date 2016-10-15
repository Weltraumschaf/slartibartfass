package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import org.junit.Test;

import static de.weltraumschaf.slartibartfass.node.Slarti.*;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link ResultAccumulator}.
 *
 * @author Sven Strittmatter
 */
public class ResultAccumulatorTest {
    private static final double DELTA = 0.0001d;

    private final ResultAccumulator sut = new ResultAccumulator(
        (l, r) -> l + r,
        (l, r) -> l + r
    );

    @Test
    public void apply_onlyIntegers() {
        assertThat(sut.result(), is(of(0L)));

        sut.apply(of(42L));

        assertThat(sut.result(), is(of(42L)));

        sut.apply(of(23L));

        assertThat(sut.result(), is(of(65L)));

        sut.apply(of(-2L));

        assertThat(sut.result(), is(of(63L)));
    }

    @Test
    public void apply_onlyReals() {
        assertThat(sut.result(), is(of(0L)));

        sut.apply(of(3.14d));

        assertThat(((SlartiReal) sut.result()).value(), is(closeTo(3.14d, DELTA)));

        sut.apply(of(1.5d));

        assertThat(((SlartiReal) sut.result()).value(), is(closeTo(4.64d, DELTA)));

        sut.apply(of(0.012d));

        assertThat(((SlartiReal) sut.result()).value(), is(closeTo(4.652d, DELTA)));
    }

    @Test
    public void apply_mixed() {
        assertThat(sut.result(), is(of(0L)));

        sut.apply(of(42L));

        assertThat(sut.result(), is(of(42L)));

        sut.apply(of(3.14d));

        assertThat(((SlartiReal) sut.result()).value(), is(closeTo(45.14d, DELTA)));

        sut.apply(of(2L));

        assertThat(((SlartiReal) sut.result()).value(), is(closeTo(47.14d, DELTA)));
    }
}

package de.weltraumschaf.slartibartfass;

import org.junit.Test;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link Pair}.
 *
 * @author Sven Strittmatter
 */
public class PairTest {

    @Test
    public void cons_oneNode() {
        final Pair sut = Pair.cons(of("foo"));

        assertThat(sut.hasCdr(), is(false));
        assertThat(sut.isEmpty(), is(false));
        assertThat(sut.car(), is(of("foo")));
        assertThat(sut.cdr(), is(not(nullValue())));
        assertThat(sut.cdr().isEmpty(), is(true));
        assertThat(sut.cdr().hasCdr(), is(false));
    }

    @Test
    public void cons_twoNodes() {
        final Pair prev = Pair.cons(of("foo"));
        final Pair sut = Pair.cons(of("bar"), prev);

        assertThat(sut.hasCdr(), is(true));
        assertThat(sut.isEmpty(), is(false));
        assertThat(sut.car(), is(of("bar")));

        assertThat(sut.cdr(), is(not(nullValue())));
        assertThat(sut.cdr().hasCdr(), is(false));
        assertThat(sut.cdr().isEmpty(), is(false));
        assertThat(sut.cdr().car(), is(of("foo")));

        assertThat(sut.cdr().cdr(), is(not(nullValue())));
        assertThat(sut.cdr().cdr().hasCdr(), is(false));
        assertThat(sut.cdr().cdr().isEmpty(), is(true));
        assertThat(sut.cdr().cdr().car(), is(nullValue()));
    }

    @Test
    public void list() {
        final Pair sut = Pair.list(of("foo"), of("bar"), of("baz"));

        assertThat(sut.car(), is(of("foo")));
        assertThat(sut.cdr().car(), is(of("bar")));
        assertThat(sut.cdr().cdr().car(), is(of("baz")));
        assertThat(sut.cdr().cdr().cdr().car(), is(nullValue()));
    }
}

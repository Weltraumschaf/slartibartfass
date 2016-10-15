package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.backend.Pair;
import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link InternalList}.
 *
 * @author Sven Strittmatter
 */
public class InternalListTest {

    private final InternalList sut = new InternalList();

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(InternalList.class)
            .withPrefabValues(Pair.class, Pair.cons(of("foo")), Pair.cons(of("bar")))
            .suppress(Warning.NONFINAL_FIELDS)
            .verify();
    }

    @Test
    public void add_size_head_tail() {
        assertThat(sut.size(), is(0));
        assertThat(sut.head(), is(nullValue()));
        assertThat(sut.tail(), is(new InternalList()));

        sut.add(of("foo"));

        assertThat(sut.size(), is(1));
        assertThat(sut.head(), is(of("foo")));
        assertThat(sut.tail(), is(new InternalList()));
        assertThat(sut, contains(of("foo")));

        sut.add(of("bar"));

        assertThat(sut.size(), is(2));
        assertThat(sut.head(), is(of("foo")));
        assertThat(sut.tail(), is(new InternalList(Collections.singletonList(of("bar")))));
        assertThat(sut, contains(of("foo"), of("bar")));

        sut.add(of("baz"));

        assertThat(sut.size(), is(3));
        assertThat(sut.head(), is(of("foo")));
        assertThat(sut.tail(), is(new InternalList(Arrays.asList(of("bar"), of("baz")))));
        assertThat(sut, contains(of("foo"), of("bar"), of("baz")));
    }

}

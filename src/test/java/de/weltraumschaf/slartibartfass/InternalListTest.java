package de.weltraumschaf.slartibartfass;

import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

public class InternalListTest {

    private final InternalList<String> sut = new InternalList<>();

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(InternalList.class)
            .withPrefabValues(InternalList.Pair.class, new InternalList.Pair<>("foo"), new InternalList.Pair<>("bar"))
            .suppress(Warning.NONFINAL_FIELDS)
            .verify();
    }

    @Test
    public void foo() {
        assertThat(sut.size(), is(0));
        assertThat(sut.head(), is(nullValue()));
        assertThat(sut.tail(), is(new InternalList<>()));

        sut.add("foo");

        assertThat(sut.size(), is(1));
        assertThat(sut.head(), is("foo"));
        assertThat(sut.tail(), is(new InternalList<>()));
        assertThat(sut, contains("foo"));

        sut.add("bar");

        assertThat(sut.size(), is(2));
        assertThat(sut.head(), is("foo"));
        assertThat(sut.tail(), is(new InternalList<>(Collections.singletonList("bar"))));
        assertThat(sut, contains("foo", "bar"));

        sut.add("baz");

        assertThat(sut.size(), is(3));
        assertThat(sut.head(), is("foo"));
        assertThat(sut.tail(), is(new InternalList<>(Arrays.asList("bar", "baz"))));
        assertThat(sut, contains("foo", "bar", "baz"));
    }

}

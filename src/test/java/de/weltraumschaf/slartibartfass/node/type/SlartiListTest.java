package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.special.SlartiSpecialForm;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiList}.
 *
 * @author Sven Strittmatter
 */
public class SlartiListTest {

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiList.class)
            .withPrefabValues(InternalList.class, new InternalList<>(Collections.singletonList("foo")), new InternalList<>(Collections.singletonList("bar")))
            .withRedefinedSubclass(SlartiSpecialForm.class)
            .verify();
    }

    @Test
    public void head() throws Exception {
        final SlartiSymbol a = sym("a");
        final SlartiSymbol b = sym("b");
        final SlartiSymbol c = sym("c");
        final SlartiList sut = list(Arrays.asList(a, b, c));

        assertThat(sut.head(), is(sameInstance(a)));
    }

    @Test
    public void tail() throws Exception {
        final SlartiSymbol a = sym("a");
        final SlartiSymbol b = sym("b");
        final SlartiSymbol c = sym("c");
        final SlartiList sut = list(Arrays.asList(a, b, c));

        assertThat(sut.tail(), is(new SlartiList(Arrays.asList(b, c))));
    }

    @Test
    public void iterator() {
        final SlartiSymbol a = sym("a");
        final SlartiSymbol b = sym("b");
        final SlartiSymbol c = sym("c");
        final SlartiList sut = list(Arrays.asList(a, b, c));

        assertThat(sut, contains(a, b, c));
    }

    @Test
    public void string() {
        final SlartiSymbol a = sym("a");
        final SlartiSymbol b = sym("b");
        final SlartiSymbol c = sym("c");
        final SlartiList sut = list(Arrays.asList(a, b, c));

        assertThat(sut.toString(), is("(a, b, c)"));
    }

    @Test
    public void value() {
        final SlartiSymbol a = sym("a");
        final SlartiSymbol b = sym("b");
        final SlartiSymbol c = sym("c");
        final SlartiList sut = list(Arrays.asList(a, b, c));

        assertThat(sut.value(), is(new InternalList<SlartiNode>(Arrays.asList(a, b, c))));
    }

    @Test
    public void castToBoolean() {
        final SlartiList sut = list(Collections.singletonList(sym("a")));

        assertThat(sut.castToBoolean(), is(SlartiBoolean.TRUE));
    }

    @Test
    public void castEMptyToBoolean() {
        assertThat(SlartiList.EMPTY.castToBoolean(), is(SlartiBoolean.FALSE));
    }

    @Test
    public void castToInteger() {
        final SlartiList sut = list(Collections.singletonList(sym("a")));

        assertThat(sut.castToInteger(), is(of(1L)));
    }

    @Test
    public void castEmptyToInteger() {
        assertThat(SlartiList.EMPTY.castToInteger(), is(of(0L)));
    }

    @Test
    public void castToReal() {
        final SlartiList sut = list(Collections.singletonList(sym("a")));

        assertThat(sut.castToReal(), is(of(1d)));
    }

    @Test
    public void castEmptyToReal() {
        assertThat(SlartiList.EMPTY.castToReal(), is(of(0d)));
    }

    @Test
    public void castToString() {
        final SlartiList sut = list(Arrays.asList(sym("a"), sym("b"), sym("c")));

        assertThat(sut.castToString(), is(of("(a, b, c)")));
    }

    @Test
    public void castEmptyToString() {
        assertThat(SlartiList.EMPTY.castToString(), is(of("()")));
    }

    @Test
    public void castToList() {
        final SlartiList sut = list(Arrays.asList(sym("a"), sym("b"), sym("c")));

        assertThat(sut.castToList(), is(sut));
    }

    @Test
    @Ignore
    public void eval() {
//        final Environment env = new Environment();
//        final SlartiFunction fn = mock(SlartiFunction.class);
//        when(a.eval(env)).thenReturn(fn);
//        when(b.eval(env)).thenReturn(new SlartiString("foo"));
//        when(c.eval(env)).thenReturn(new SlartiString("bar"));
//
//        sut.eval(env);
//
//        verify(fn, times(1)).apply(Arrays.asList(new SlartiString("foo"), new SlartiString("bar")));
    }

}
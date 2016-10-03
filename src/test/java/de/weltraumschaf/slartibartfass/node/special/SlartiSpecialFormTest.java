package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link SlartiSpecialForm}.
 *
 * @author Sven Strittmatter
 */
public class SlartiSpecialFormTest {
    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiSpecialForm.class)
            .withRedefinedSuperclass()
            .withPrefabValues(InternalList.class, new InternalList<>(Collections.singletonList("foo")), new InternalList<>(Collections.singletonList("bar")))
            .verify();
    }

    @Test
    public void check_empty() {
        assertThat(SlartiSpecialForm.check(SlartiList.NIL), is(SlartiList.NIL));
    }

    @Test
    public void check_define() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("define"), new SlartiInteger(23L), new SlartiSymbol("foo")))),
            is(new DefineSpecialForm(new SlartiList(Arrays.asList(new SlartiInteger(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_lambda() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("lambda"), new SlartiInteger(23L), new SlartiSymbol("foo")))),
            is(new LambdaSpecialForm(new SlartiList(Arrays.asList(new SlartiInteger(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_if() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("if"), new SlartiInteger(23L), new SlartiSymbol("foo")))),
            is(new IfSpecialForm(new SlartiList(Arrays.asList(new SlartiInteger(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_quote() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("quote"), new SlartiInteger(23L), new SlartiSymbol("foo")))),
            is(new QuoteSpecialForm(new SlartiList(Arrays.asList(new SlartiInteger(23L), new SlartiSymbol("foo"))))));
    }
}

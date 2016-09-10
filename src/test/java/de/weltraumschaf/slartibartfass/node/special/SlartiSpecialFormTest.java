package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.node.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNumber;
import de.weltraumschaf.slartibartfass.node.SlartiSymbol;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiSpecialFormTest {
    @Test
    @Ignore
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(SlartiSpecialForm.class).verify();
    }

    @Test
    public void check_empty() {
        assertThat(SlartiSpecialForm.check(SlartiList.EMPTY), is(SlartiList.EMPTY));
    }

    @Test
    public void check_define() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("define"), new SlartiNumber(23L), new SlartiSymbol("foo")))),
            is(new DefineSpecialForm(new SlartiList(Arrays.asList(new SlartiNumber(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_lambda() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("lambda"), new SlartiNumber(23L), new SlartiSymbol("foo")))),
            is(new LambdaSpecialForm(new SlartiList(Arrays.asList(new SlartiNumber(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_if() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("if"), new SlartiNumber(23L), new SlartiSymbol("foo")))),
            is(new IfSpecialForm(new SlartiList(Arrays.asList(new SlartiNumber(23L), new SlartiSymbol("foo"))))));
    }

    @Test
    public void check_quote() {
        assertThat(
            SlartiSpecialForm.check(new SlartiList(Arrays.asList(new SlartiSymbol("quote"), new SlartiNumber(23L), new SlartiSymbol("foo")))),
            is(new QuoteSpecialForm(new SlartiList(Arrays.asList(new SlartiNumber(23L), new SlartiSymbol("foo"))))));
    }
}

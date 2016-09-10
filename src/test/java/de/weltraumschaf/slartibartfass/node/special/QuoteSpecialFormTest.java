package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNumber;
import de.weltraumschaf.slartibartfass.node.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class QuoteSpecialFormTest {

    @Test
    public void eval() {
        final SlartiSpecialForm sut = new QuoteSpecialForm(new SlartiList(
            new SlartiSymbol("foo"), new SlartiSymbol("bar"), new SlartiNumber(23L)
        ));

        assertThat(sut.eval(new Environment()), is("foo bar 23"));
    }
}

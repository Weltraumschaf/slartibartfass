package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class QuoteSpecialFormTest {

    @Test
    public void eval() {
        final SlartiSpecialForm sut = new QuoteSpecialForm(new SlartiList(
            new SlartiSymbol("foo"), new SlartiSymbol("bar"), new SlartiInteger(23L)
        ));

        assertThat(sut.eval(new Environment()), is("foo bar 23"));
    }
}

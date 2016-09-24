package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

public class QuoteSpecialFormTest {

    @Test
    public void eval() {
        final SlartiSpecialForm sut = new QuoteSpecialForm(of(
            new SlartiSymbol("foo"), of("bar"), of(23L)
        ));

        final Environment env = new Environment();
        env.putValue("foo", of(42L));

        assertThat(sut.eval(env), is(of(
            new SlartiSymbol("foo"), of("bar"), of(23L)
        )));
    }
}

package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiNumber;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class DefineSpecialFormTest {
    @Test
    public void eval() {
        final Environment env = new Environment();
        final SlartiSpecialForm sut = new DefineSpecialForm(new SlartiList(
            Arrays.asList(new SlartiSymbol("foo"), new SlartiNumber(42L))
        ));

        assertThat(sut.eval(env), is(SlartiList.EMPTY));

        assertThat(env.getValue("foo"), is(42L));
    }
}

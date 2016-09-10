package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.*;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import java.util.Arrays;

public class LambdaSpecialFormTest {

    @Test
    public void eval() {
        final Environment env = new Environment();
        SlartiBuiltinFunction.register(env);
        final SlartiSpecialForm sut = new LambdaSpecialForm(new SlartiList(
            new SlartiList(new SlartiSymbol("x")),
            new SlartiList(new SlartiSymbol("*"), new SlartiSymbol("x"), new SlartiSymbol("x"))
        ));

        final SlartiFunction function = (SlartiFunction) sut.eval(env);

        final Object result = function.apply(Arrays.asList(new SlartiNumber(10L)));

        assertThat(result, is(100L));
    }

}

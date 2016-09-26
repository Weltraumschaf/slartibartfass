package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

import java.util.Arrays;

/**
 * Tests for {@link LambdaSpecialForm}.
 *
 * @author Sven Strittmatter
 */
public class LambdaSpecialFormTest {

    @Test
    public void eval() {
        final Environment env = new Environment();
        SlartiBuiltinFunctions.register(env, mock(IO.class));
        final SlartiSpecialForm sut = new LambdaSpecialForm(new SlartiList(
            new SlartiList(new SlartiSymbol("x")),
            new SlartiList(new SlartiSymbol("*"), new SlartiSymbol("x"), new SlartiSymbol("x"))
        ));

        final SlartiFunction function = (SlartiFunction) sut.eval(env);

        final Object result = function.apply(Arrays.asList(new SlartiInteger(10L)));

        assertThat(result, is(new SlartiInteger(100L)));
    }

}

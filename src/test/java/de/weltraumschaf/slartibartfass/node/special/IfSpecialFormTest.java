package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNumber;
import de.weltraumschaf.slartibartfass.node.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class IfSpecialFormTest {

    private final Environment env = new Environment();

    @Test
    public void eval_evalThenBranchIfConditionIsTrue() {
        env.putValue("cond", Boolean.TRUE);
        final SlartiSpecialForm sut = new IfSpecialForm(new SlartiList(
            new SlartiSymbol("cond"),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiNumber(42L))),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiNumber(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue("res"), is(42L));
    }

    @Test
    public void eval_evalElseBranchIfConditionIsFalse() {
        env.putValue("cond", Boolean.FALSE);
        final SlartiSpecialForm sut = new IfSpecialForm(new SlartiList(
            new SlartiSymbol("cond"),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiNumber(42L))),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiNumber(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue("res"), is(23L));
    }
}

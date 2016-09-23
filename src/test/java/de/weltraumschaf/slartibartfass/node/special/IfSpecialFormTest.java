package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class IfSpecialFormTest {

    private final Environment env = new Environment();

    @Test
    public void eval_evalThenBranchIfConditionIsTrue() {
        env.putValue("cond", SlartiBoolean.TRUE);
        final SlartiSpecialForm sut = new IfSpecialForm(new SlartiList(
            new SlartiSymbol("cond"),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiInteger(42L))),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiInteger(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue("res"), is(new SlartiInteger(42L)));
    }

    @Test
    public void eval_evalElseBranchIfConditionIsFalse() {
        env.putValue("cond", SlartiBoolean.FALSE);
        final SlartiSpecialForm sut = new IfSpecialForm(new SlartiList(
            new SlartiSymbol("cond"),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiInteger(42L))),
            new DefineSpecialForm(new SlartiList(new SlartiSymbol("res"), new SlartiInteger(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue("res"), is(new SlartiInteger(23L)));
    }
}

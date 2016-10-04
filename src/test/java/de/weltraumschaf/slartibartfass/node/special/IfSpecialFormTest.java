package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.MemoryBox;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import org.junit.Test;

import static de.weltraumschaf.slartibartfass.node.Slarti.*;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link IfSpecialForm}.
 *
 * @author Sven Strittmatter
 */
public class IfSpecialFormTest {

    private final Environment env = new Environment();

    @Test(expected = SlartiError.class)
    public void eval_noArguments() {
        final SlartiSpecialForm sut = new IfSpecialForm(SlartiList.NIL);

        sut.eval(env);
    }

    @Test(expected = SlartiError.class)
    public void eval_onlyCondition() {
        final SlartiSpecialForm sut = new IfSpecialForm(list(sym("cond")));

        sut.eval(env);
    }

    @Test(expected = SlartiError.class)
    public void eval_toManyArguments() {
        final SlartiSpecialForm sut = new IfSpecialForm(list(sym("cond"), sym("then"), sym("else"), sym("too-much")));

        sut.eval(env);
    }

    @Test
    public void eval_evalThenBranchIfConditionIsTrue() {
        env.putValue(sym("cond"), SlartiBoolean.TRUE);
        final SlartiSpecialForm sut = new IfSpecialForm(list(
            sym("cond"),
            new DefineSpecialForm(list(sym("res"), of(42L))),
            new DefineSpecialForm(list(sym("res"), of(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue(sym("res")), is(new MemoryBox(sym("res"), of(42L))));
    }

    @Test
    public void eval_evalElseBranchIfConditionIsFalse() {
        env.putValue(sym("cond"), SlartiBoolean.FALSE);
        final SlartiSpecialForm sut = new IfSpecialForm(list(
            sym("cond"),
            new DefineSpecialForm(list(sym("res"), of(42L))),
            new DefineSpecialForm(list(sym("res"), of(23L)))
        ));

        sut.eval(env);

        assertThat(env.getValue(sym("res")), is(new MemoryBox(sym("res"), of(23L))));
    }

    @Test
    public void eval_noElseBranchConditionIsTrue() {
        final SlartiSpecialForm sut = new IfSpecialForm(list(of(true), of(42L)));

        assertThat(sut.eval(env), is(of(42L)));
    }

    @Test
    public void eval_noElseBranchConditionIsFalse() {
        final SlartiSpecialForm sut = new IfSpecialForm(list(of(false), of(42L)));

        assertThat(sut.eval(env), is(SlartiList.NIL));
    }

}

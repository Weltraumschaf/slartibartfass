package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;
import org.junit.Ignore;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

public class ApplicationTest {
    private final Application sut = new Application(new String[0], mock(IO.class));

    @Test
    public void loadBuiltInFunctions() {
        final Environment env = new Environment();

        sut.loadBuiltInFunctions(env);

        assertThat(env.size(), is(SlartiBuiltinFunctions.values().length));

        for (final SlartiBuiltinFunctions fn : SlartiBuiltinFunctions.values()) {
            assertThat(String.format(
                "Built in function %s not in env!", fn),
                env.getValue(fn.impl().name()),
                is(fn.impl()));
        }
    }

    @Test
    public void loadStdLib() throws IOException {
        final Environment env = new Environment();

        sut.loadStdLib(new DefaultSlartiVisitor(), env);

        assertThat(env.size(), is(5));
    }
}

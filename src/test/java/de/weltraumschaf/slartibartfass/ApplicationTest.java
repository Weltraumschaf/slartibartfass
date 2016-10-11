package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.frontend.DefaultSlartiVisitor;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;

public class ApplicationTest {
    private final Application sut = new Application(new String[0]);

    @Before
    public void setIo() throws IOException {
        sut.setIoStreams(mock(IO.class));
        sut.prepareExecution();
    }

    @Test
    @Ignore
    public void writeTests() {

    }
}

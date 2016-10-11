package de.weltraumschaf.slartibartfass;

import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Test;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for {@link CliOptions}.
 *
 * @author Sven Strittmatter
 */
public class CliOptionsTest {

    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(CliOptions.class).suppress(Warning.NONFINAL_FIELDS).verify();
    }

    @Test
    public void defaults() {
        final CliOptions sut = new CliOptions();

        assertThat(sut.isDebug(), is(false));
        assertThat(sut.isHelp(), is(false));
        assertThat(sut.isVersion(), is(false));
        assertThat(sut.getFiles(), hasSize(0));
    }
}

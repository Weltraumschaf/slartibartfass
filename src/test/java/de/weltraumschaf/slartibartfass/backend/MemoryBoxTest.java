package de.weltraumschaf.slartibartfass.backend;

import nl.jqno.equalsverifier.EqualsVerifier;
import nl.jqno.equalsverifier.Warning;
import org.junit.Test;

import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link MemoryBox}.
 */
public class MemoryBoxTest {
    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(MemoryBox.class).suppress(Warning.NONFINAL_FIELDS).verify();
    }

    @Test(expected = NullPointerException.class)
    public void constructWithValueAsNullIsNotAllowed() {
        new MemoryBox(null);
    }

    @Test(expected = NullPointerException.class)
    public void setValueWithNullIsnNotAlowed() {
        new MemoryBox(of(42L)).memory(null);
    }
}

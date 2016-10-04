package de.weltraumschaf.slartibartfass;

import nl.jqno.equalsverifier.EqualsVerifier;
import org.junit.Test;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link MemoryBox}.
 */
public class MemoryBoxTest {
    @Test
    public void equalsAndHashCode() {
        EqualsVerifier.forClass(MemoryBox.class).verify();
    }

    @Test(expected = NullPointerException.class)
    public void constructWithSymbolAsNullIsNotAllowed() {
        new MemoryBox(null, of(42L));
    }

    @Test(expected = NullPointerException.class)
    public void constructWithValueAsNullIsNotAllowed() {
        new MemoryBox(sym("symbol"), null);
    }

    @Test(expected = NullPointerException.class)
    public void setValueWithNullIsnNotAlowed() {
        new MemoryBox(sym("symbol"), of(42L)).memory(null);
    }
}

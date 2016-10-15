package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.type.*;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiType}.
 */
public class SlartiTypeTest {
    @Rule
    public final ExpectedException thrown = ExpectedException.none();
    private final SlartiType sut = new SlartiType() {
    };

    @Test
    public void castTo_wantedIsBoolean() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage(SlartiBoolean.class.getSimpleName());

        sut.castTo(SlartiBoolean.TRUE);
    }

    @Test
    public void castTo_wantedIsInteger() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage(SlartiInteger.class.getSimpleName());

        sut.castTo(of(42L));
    }

    @Test
    public void castTo_wantedIsReal() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage(SlartiReal.class.getSimpleName());

        sut.castTo(of(3.14d));
    }

    @Test
    public void castTo_wantedIsString() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage(SlartiString.class.getSimpleName());

        sut.castTo(of("foo"));
    }

    @Test
    public void castTo_wantedIsList() {
        thrown.expect(SlartiError.class);
        thrown.expectMessage(SlartiList.class.getSimpleName());

        sut.castTo(list(of(42L)));
    }
}

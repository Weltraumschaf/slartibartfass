package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.frontend.SlartiBaseVisitor;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.special.QuoteSpecialForm;
import de.weltraumschaf.slartibartfass.node.special.SlartiSpecialForm;
import de.weltraumschaf.slartibartfass.node.type.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Default implementation which converts the parsed tree into {@link SlartiNode nodes}.
 *
 * @author Sven Strittmatter
 */
final class DefaultSlartiVisitor extends SlartiBaseVisitor<SlartiNode> {

    @Override
    protected SlartiNode defaultResult() {
        return SlartiList.EMPTY;
    }

    @Override
    public SlartiNode visitFile(final SlartiParser.FileContext ctx) {
        return new SlartiList(
            ctx.form()
                .stream()
                .map(this::visit)
                .collect(Collectors.toCollection((Supplier<Collection<SlartiNode>>) ArrayList::new)));
    }

    @Override
    public SlartiNode visitList(final SlartiParser.ListContext ctx) {
        Collection<SlartiNode> list = ctx.form()
            .stream()
            .map(this::visit)
            .collect(Collectors.toCollection(ArrayList::new));

        return SlartiSpecialForm.check(new SlartiList(list));
    }

    @Override
    public SlartiNode visitInteger(final SlartiParser.IntegerContext ctx) {
        return new SlartiInteger(Long.valueOf(ctx.getText(), 10));
    }

    @Override
    public SlartiNode visitReal(final SlartiParser.RealContext ctx) {
        return new SlartiReal(Double.valueOf(ctx.getText()));
    }

    @Override
    public SlartiNode visitBool(final SlartiParser.BoolContext ctx) {
        if ("#true".equals(ctx.getText())) {
            return SlartiBoolean.TRUE;
        }

        return SlartiBoolean.FALSE;
    }

    @Override
    public SlartiNode visitSymbol(final SlartiParser.SymbolContext ctx) {
        return new SlartiSymbol(ctx.getText());
    }

    @Override
    public SlartiNode visitQuote(final SlartiParser.QuoteContext ctx) {
        return new QuoteSpecialForm(visit(ctx.form()).castToList());
    }

    @Override
    public SlartiNode visitString(final SlartiParser.StringContext ctx) {
        final String text = ctx.getText();
        final StringBuilder buffer = new StringBuilder();

        for (int i = 1; i < text.length() - 1; ++i) {
            final char c = text.charAt(i);

            if (c == '\\') {
                char next = text.charAt(i + 1);
                ++i;

                switch (next) {
                    case '\\':
                        buffer.append('\\');
                        break;
                    case '"':
                        buffer.append('"');
                        break;
                    case 'n':
                        buffer.append('\n');
                        break;
                    case 'r':
                        buffer.append('\r');
                        break;
                    case 't':
                        buffer.append('\t');
                        break;
                    case 'f':
                        buffer.append('\f');
                        break;
                    case 'b':
                        buffer.deleteCharAt(buffer.length() -1);
                        break;
                    default:
                        buffer.append(next);
                        break;
                }
            } else {
                buffer.append(c);
            }
        }

        return new SlartiString(buffer.toString());
    }
}

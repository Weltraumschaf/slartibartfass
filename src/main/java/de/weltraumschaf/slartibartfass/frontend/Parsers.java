package de.weltraumschaf.slartibartfass.frontend;


import org.antlr.v4.runtime.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;

/**
 * Factory to fmt parser.
 *
 * @author Sven Strittmatter
 */
public class Parsers {

    /**
     * Creates a new parser instance.
     *
     * @param src must not be {@code null}
     * @return never {@code null} always new instance
     * @throws IOException if the source can't be read
     */
    public SlartiParser newParser(final InputStream src) throws IOException {
        Objects.requireNonNull(src, "Parameter 'src' must not be null!");
        final CharStream input = new ANTLRInputStream(src);
        final Lexer lexer = new SlartiLexer(input);
        final TokenStream tokens = new CommonTokenStream(lexer);

        return new SlartiParser(tokens);
    }
}

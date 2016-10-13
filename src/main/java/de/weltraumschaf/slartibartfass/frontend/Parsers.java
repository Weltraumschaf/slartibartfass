package de.weltraumschaf.slartibartfass.frontend;


import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.frontend.SlartiLexer;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.Parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.file.Path;
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
     * @return never {@code null} alsways new instance
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

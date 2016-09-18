package de.weltraumschaf.slartibartfass;


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

public final class Parsers {

    private final IO io;

    public Parsers(final IO io) {
        super();
        this.io = Objects.requireNonNull(io, "Parameter 'io' must not be null!");
    }

    public SlartiParser newParser(final InputStream src, final boolean debugEnabled) throws IOException {
        Objects.requireNonNull(src, "Parameter 'src' must not be null!");
        final CharStream input = new ANTLRInputStream(src);
        final Lexer lexer = new SlartiLexer(input);
        final TokenStream tokens = new CommonTokenStream(lexer);
        final SlartiParser parser = new SlartiParser(tokens);

        parser.removeErrorListeners();
        parser.addErrorListener(new ErrorListener(io, debugEnabled));
//        parser.setErrorHandler(new BailErrorStrategy());

        return parser;
    }
}

package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.slartibartfass.SlartiInputOutput;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;

/**
 * @author Sven Strittmatter
 */
public final class Interpreter extends BaseExecutor {

    public Interpreter(final SlartiInputOutput output) {
        super(output);
    }

    public void start(final Collection<String> filenames) {
        init();
        filenames.forEach(filename -> {
            output.debug("Interpret file %s  ...", filename);
            final SlartiParser parser;

            try (FileInputStream src = new FileInputStream(filename)) {
                parser = parsers.newParser(src);
            } catch (IOException e) {
                throw new SlartiError("Can't read sourc file! Reason: %s", e.getMessage());
            }

            visitor.visit(parser.file()).eval(env);
        });
    }
}

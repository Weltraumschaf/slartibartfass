package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.SlartiInputOutput;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;

/**
 * Interprets all given files.
 *
 * @author Sven Strittmatter
 */
final class Interpreter extends BaseExecutor {

    /**
     * Files to interpret.
     */
    private final Collection<String> filenames;

    /**
     * Dedicated constructor.
     *
     * @param io must not be {@code null}
     * @param filenames must not be {@code null}
     */
    Interpreter(final SlartiInputOutput io, final Collection<String> filenames) {
        super(io);
        this.filenames = Validate.notNull(filenames, "filenames");
    }

    @Override
    public void start() {
        filenames.forEach(filename -> {
            io().debug("Interpret file %s  ...", filename);
            final SlartiParser parser;

            try (FileInputStream src = new FileInputStream(filename)) {
                parser = parsers().newParser(src);
            } catch (IOException e) {
                throw new SlartiError("Can't read source file! Reason: %s", e.getMessage());
            }

            visitor().visit(parser.file()).eval(env());
        });
    }
}

package de.weltraumschaf.slartibartfass;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.Interval;

import java.io.PrintStream;
import java.util.BitSet;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public final  class ErrorListener implements ANTLRErrorListener {

    private final PrintStream out;
    private final PrintStream err;
    /**
     * If {@code false} only {@link #error(java.lang.String) errors} are printed out, if {@code true} everything is
     * printed out.
     */
    private final boolean debugEnabled;

    /**
     * Dedicated constructor.
     *
     * @param io must not be {@code null}
     */
    public ErrorListener(final PrintStream out, final PrintStream err, final boolean debugEnabled) {
        super();
        this.out = Objects.requireNonNull(out, "Parameter 'out' must not be null!");
        this.err = Objects.requireNonNull(err, "Parameter 'out' must not be null!");
        this.debugEnabled = debugEnabled;
    }

    /**
     * Writes an error.
     *
     * @param msg must not be {@code null} or empty
     */
    private void error(final String msg) {
        err.print("[E] ");
        err.println(msg);
    }

    /**
     * Writes an information.
     *
     * @param msg must not be {@code null} or empty
     */
    private void information(final String msg) {
        if (debugEnabled) {
            out.print("[I] ");
            out.println(msg);
        }
    }

    @Override
    public void syntaxError(
        final Recognizer<?, ?> recognizer,
        final Object offendingSymbol,
        final int line,
        final int charPositionInLine,
        final String msg,
        final RecognitionException e) {
        final List<String> stack = ((Parser) recognizer).getRuleInvocationStack();
        Collections.reverse(stack);
        information(String.format("Rule stack: %s", stack));
        error(String.format("Syntax error at line %d column %d: %s", line, charPositionInLine + 1, msg));
    }

    @Override
    public void reportAmbiguity(
        final Parser recognizer,
        final DFA dfa,
        final int startIndex,
        final int stopIndex,
        final boolean exact,
        final BitSet ambigAlts,
        final ATNConfigSet configs) {
        final String decision = getDecisionDescription(recognizer, dfa);
        final BitSet conflictingAlts = getConflictingAlts(ambigAlts, configs);
        final String input = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
        information(String.format(
            "Syntax ambiguity starting at index %d identified at index %d: decision=%s: conflictingAlts=%s, input='%s'.",
            startIndex, stopIndex, decision, conflictingAlts, input));
    }

    @Override
    public void reportAttemptingFullContext(
        final Parser recognizer,
        final DFA dfa,
        final int startIndex,
        final int stopIndex,
        final BitSet conflictingAlts,
        final ATNConfigSet configs) {
        final String decision = getDecisionDescription(recognizer, dfa);
        final String input = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
        information(String.format(
            "Attempting full context starting at index %d identified at index %d: decision=%s, input='%s'.",
            startIndex, stopIndex, decision, input));
    }

    @Override
    public void reportContextSensitivity(
        final Parser recognizer,
        final DFA dfa,
        final int startIndex,
        final int stopIndex,
        final int prediction,
        final ATNConfigSet configs) {
        final String decision = getDecisionDescription(recognizer, dfa);
        final String input = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
        information(String.format(
            "Context sensitivity starting at index %d identified at index %d: decision=%s, input='%s'.",
            startIndex, stopIndex, decision, input));
    }

    private String getDecisionDescription(final Parser recognizer, final DFA dfa) {
        final int decision = dfa.decision;
        final int ruleIndex = dfa.atnStartState.ruleIndex;

        final String[] ruleNames = recognizer.getRuleNames();

        if (ruleIndex < 0 || ruleIndex >= ruleNames.length) {
            return String.valueOf(decision);
        }

        final String ruleName = ruleNames[ruleIndex];

        if (ruleName == null || ruleName.isEmpty()) {
            return String.valueOf(decision);
        }

        return String.format("%d (%s)", decision, ruleName);
    }

    /**
     * Computes the set of conflicting or ambiguous alternatives from a configuration set, if that information was not
     * already provided by the parser.
     *
     * @param reportedAlts The set of conflicting or ambiguous alternatives, as reported by the parser.
     * @param configs The conflicting or ambiguous configuration set.
     * @return Returns {@code reportedAlts} if it is not {@code null}, otherwise returns the set of alternatives
     * represented in {@code configs}.
     */
    protected BitSet getConflictingAlts(final BitSet reportedAlts, final ATNConfigSet configs) {
        if (reportedAlts != null) {
            return reportedAlts;
        }

        final BitSet result = new BitSet();
        configs.stream().forEach((config) -> {
            result.set(config.alt);
        });

        return result;
    }

}
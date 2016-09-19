package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

/**
 * Base type for AST nodes.
 */
public interface SlartiNode {
    /**
     * Interprets the node.
     * <p>
     *     To evaluate the node it needs an {@link Environment} from the calling context. Every node may
     *     add entries into the environment or add child environments and pass them to evaluated child nodes
     *     to give them an own scope.
     * </p>
     * <p>
     *     The method returns the result of the evaluation.
     * </p>
     *
     * @param env must not be {@code null}
     * @return never {@code null}
     */
    Object eval(Environment env);
}

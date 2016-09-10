package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

public interface SlartiNode {
    Object eval(Environment env);
}

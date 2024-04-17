package io.github.nextentity.core.meta.graph;

import io.github.nextentity.core.expression.Attribute;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public interface EntityProperty extends Graph, Attribute, Property {

    String columnName();

    boolean isVersion();

    EntitySchema declaringType();

    @Override
    EntityProperty get(String path);

    @Override
    EntityProperty get(Attribute column);

    @NotNull
    @Override
    default Iterator<String> iterator() {
        return referencedAttributes().stream().map(EntityProperty::name).iterator();
    }

    @Override
    default EntityProperty toAttribute(EntitySchema entityType) {
        return this;
    }

    @Override
    default int deep() {
        return referencedAttributes().size();
    }

    @Override
    default String get(int i) {
        return referencedAttributes().get(i).name();
    }

    @Override
    default EntityProperty subLength(int len) {
        return referencedAttributes().get(len - 1);
    }

    @Override
    default EntityProperty parent() {
        return declaringType() instanceof EntityProperty ? (EntityProperty) declaringType() : null;
    }

    static Graph getDeclaringType(Graph type) {
        if (type instanceof EntityProperty) {
            return ((EntityProperty) type).declaringType();
        }
        return null;
    }

    default List<? extends EntityProperty> referencedAttributes() {
        Graph cur = this;
        ArrayDeque<EntityProperty> attributes = new ArrayDeque<>(2);
        while (true) {
            if (cur instanceof EntityProperty) {
                // noinspection PatternVariableCanBeUsed
                EntityProperty attribute = (EntityProperty) cur;
                attributes.addFirst(attribute);
                cur = attribute.declaringType();
            } else {
                break;
            }
        }
        // noinspection Java9CollectionFactory
        return Collections.unmodifiableList(new ArrayList<>(attributes));
    }

}

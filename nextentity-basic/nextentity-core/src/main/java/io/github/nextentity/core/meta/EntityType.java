package io.github.nextentity.core.meta;

public interface EntityType extends ObjectType {

    Attribute id();

    String tableName();

    Attribute getAttribute(String fieldName);

    Attribute version();

}

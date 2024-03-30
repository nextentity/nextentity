package io.github.nextentity.core.meta;

public interface AnyToOneAttribute extends Attribute, EntityType {

    String joinColumnName();

    String joinName();

    String referencedColumnName();

}

package io.github.nextentity.test.projection;

import io.github.nextentity.core.meta.graph.ProjectionSchema;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import io.github.nextentity.test.entity.User;

/**
 * @author HuangChengwei
 * @since 2024/4/18 下午4:27
 */
class IUserTest {

    public static void main(String[] args) {
        ProjectionSchema projection = JpaMetamodel.of().getProjection(User.class, IUser.class);
        ProjectionSchema parentUser = (ProjectionSchema) projection.getProperty("parentUser");
        System.out.println(parentUser.properties());
    }

}
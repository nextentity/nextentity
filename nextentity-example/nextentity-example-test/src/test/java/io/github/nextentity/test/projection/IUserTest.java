package io.github.nextentity.test.projection;

import io.github.nextentity.core.meta.ProjectionType;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import io.github.nextentity.test.entity.User;

/**
 * @author HuangChengwei
 * @since 2024/4/18 下午4:27
 */
class IUserTest {

    public static void main(String[] args) {
        ProjectionType projection = JpaMetamodel.of().getEntity(User.class).getProjection(IUser.class);
        ProjectionType parentUser = (ProjectionType) projection.getAttribute("parentUser");
        System.out.println(parentUser.attributes());
    }

}
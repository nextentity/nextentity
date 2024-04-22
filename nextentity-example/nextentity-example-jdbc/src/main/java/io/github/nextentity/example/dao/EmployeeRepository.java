package io.github.nextentity.example.dao;

import io.github.nextentity.data.AbstractRepository;
import io.github.nextentity.example.eneity.Employee;
import org.springframework.stereotype.Repository;

/**
 * @author HuangChengwei
 * @since 2024/4/15 下午12:14
 */
@Repository
public class EmployeeRepository extends AbstractRepository<Integer, Employee> {

}

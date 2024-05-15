package io.github.nextentity.example.dao;

import io.github.nextentity.api.Repository;
import io.github.nextentity.example.eneity.Company;
import io.github.nextentity.example.eneity.Employee;
import io.github.nextentity.example.eneity.User;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/**
 * @author HuangChengwei
 * @since 2024-03-27 14:26
 */
@SpringBootTest
class CompanyRepositoryTest {
    @Autowired(required = false)
    CompanyRepository companyRepository;
    @Autowired(required = false)
    Repository<Integer, Company> companyEntities;
    @Autowired(required = false)
    EmployeeRepository employeeRepository;
    @Autowired(required = false)
    Repository<Integer, Employee> employeeRepository2;
    @Autowired(required = false)
    Repository<Integer, Employee> employeeEntities;
    @Autowired(required = false)
    Repository<Long, User> userRepository;

    @Test
    void testAutowired() {
        Assertions.assertNotNull(employeeEntities);
        Assertions.assertNotNull(companyRepository);
        Assertions.assertNotNull(companyEntities);
        Assertions.assertNotNull(employeeRepository);
        Assertions.assertNotNull(employeeRepository2);
        Assertions.assertNotNull(userRepository);
    }

}
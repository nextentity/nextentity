package io.github.nextentity.example.dao;

import io.github.nextentity.api.Repository;
import io.github.nextentity.example.eneity.Company;
import io.github.nextentity.example.eneity.Employee;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/**
 * @author HuangChengwei
 * @since 2024-03-27 14:26
 */
@Slf4j
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

    @Test
    void testAutowired() {
        Assertions.assertNotNull(employeeRepository);
        Assertions.assertNotNull(companyRepository);
        Assertions.assertNotNull(companyRepository);
        Assertions.assertNotNull(employeeRepository);
        Assertions.assertNotNull(employeeRepository2);
    }


    @Test
    void testFindById() {
        Company company = companyRepository.get(1);
        log.info("{}", company);

        Employee employee = employeeRepository.fetch(Employee::getCompany).getFirst();
        log.info("{}", employee);

        Company company1 = employeeRepository.select(Employee::getCompany).getFirst();
        log.info("{}", company1);


    }

}
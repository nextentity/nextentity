package io.github.nextentity.example.service;

import io.github.nextentity.api.Repository;
import io.github.nextentity.example.dao.CompanyRepository;
import io.github.nextentity.example.eneity.Company;
import io.github.nextentity.example.eneity.Employee;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 数据库初始化器
 *
 * @author HuangChengwei
 * @since 2024-04-29 11:22
 */
@Component
@RequiredArgsConstructor
public class DatabaseInitializer implements InitializingBean {

    private final CompanyRepository companyRepository;
    private final Repository<Integer, Employee> employeeRepository;

    @Override
    public void afterPropertiesSet() {
        List<Employee> list = employeeRepository.getList();
        employeeRepository.delete(list);
        companyRepository.delete(companyRepository.getList());
        AtomicInteger companyIds = new AtomicInteger();
        AtomicInteger employeeIds = new AtomicInteger();
        Company company = new Company();

        company.setId(companyIds.incrementAndGet());
        company.setName("Next Entity");
        company.setAddr("guangzhou city guangdong province");
        companyRepository.insert(company);


        Employee employee = new Employee();
        employee.setId(employeeIds.incrementAndGet());
        employee.setName("Zhang San");
        employee.setAge(21);
        employee.setCompanyId(companyIds.get());

        employeeRepository.insert(employee);

    }
}

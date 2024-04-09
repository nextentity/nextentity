package io.github.nextentity.example.dao;

import io.github.nextentity.core.api.Entities;
import io.github.nextentity.example.eneity.Company;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Objects;

/**
 * @author HuangChengwei
 * @since 2024-03-27 14:26
 */
@SpringBootTest
class CompanyRepositoryTest {
    @Autowired
    CompanyRepository companyRepository;
    @Autowired
    Entities<Integer, Company> companyEntities;

    @Test
    void test() {
        System.out.println(Objects.equals(companyRepository, companyEntities));
    }
}
package io.github.nextentity.example;

import io.github.nextentity.api.Repository;
import io.github.nextentity.example.entity.Article;
import io.github.nextentity.example.entity.Author;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@SpringBootTest
public class ExampleTest {

    @Autowired
    Repository<Integer, Author> authorRepository;
    @Autowired
    Repository<Integer, Article> articleRepository;

    @Test
    @Order(0)
    void insert() {
        Author author = new Author();
        author.setId(1);
        author.setAge(26);
        author.setFirstName("Michael");
        author.setLastName("Jordan");
        // sql: insert into `author` (`id`,`first_name`,`last_name`,`age`) values (?,?,?,?)
        // parameters: 1, Michael, Jordan, 26
        authorRepository.insert(author);

        Article article = new Article();

        article.setId(1);
        article.setTitle("nextentity examples");
        article.setDescription("A few examples of nextentity CRUD operations");
        article.setAuthorId(author.getId());
        // sql: insert into `article` (`id`,`title`,`description`,`author_id`) values (?,?,?,?)
        // parameters: 1, nextentity examples, A few examples of nextentity CRUD operations, 1
        articleRepository.insert(article);
    }

    @Test
    @Order(1)
    void update() {
        Article article = articleRepository.get(1);
        if (article != null) {
            article.setDescription("A few examples of nextentity CRUD operations updated");
            // sql: update `article` set `title`=?,`description`=?,`author_id`=? where `id`=?
            // parameters: nextentity examples, A few examples of nextentity CRUD operations updated, 1, 1
            articleRepository.update(article);
        }
    }

    @Test
    @Order(2)
    void query() {
        // select a_.`id`,a_.`title`,a_.`description`,a_.`author_id`
        // from `article` a_ where a_.`title`='nextentity examples'
        List<Article> articles = articleRepository
                .where(Article::getTitle).eq("nextentity examples")
                .getList();
        // [Article(id=1, title=nextentity examples, description=A few examples of nextentity CRUD operations updated,
        // authorId=1, author=null)]
        log.info("{}", articles);

        // select
        // a_.`id`,a_.`title`,a_.`description`,a_.`author_id`,a0_.`id` as _0,
        // a0_.`first_name` as _1,a0_.`last_name` as _2,a0_.`age` as _3
        // from `article` a_ left join `author` a0_ on a_.author_id=a0_.id
        // where a_.`title`='nextentity examples'
        articles = articleRepository
                .fetch(Article::getAuthor)
                .where(Article::getTitle).eq("nextentity examples")
                .getList();
        // [Article(id=1, title=nextentity examples, description=A few examples of nextentity CRUD operations updated,
        // authorId=1, author=Author(id=1, firstName=Michael, lastName=Jordan, age=26))]
        log.info("{}", articles);

    }

    @Test
    @Order(3)
    void delete() {
        Article article = articleRepository.get(1);
        // sql: delete from `article` where `id`=?
        // parameters: 1
        articleRepository.delete(article);
        Author author = authorRepository.get(1);
        // sql: delete from `author` where `id`=?
        // parameters: 1
        authorRepository.delete(author);
    }
}

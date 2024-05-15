# spring boot example

## Maven Dependencies

```xml

<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>

  <groupId>io.github.nextentity</groupId>
  <artifactId>nextentity-example-jdbc</artifactId>

  <dependencies>
    <dependency>
      <groupId>io.github.nextentity</groupId>
      <artifactId>spring-boot-starter-nextentity-jdbc</artifactId>
      <version>${project.version}</version>
    </dependency>

    <dependency>
      <groupId>com.mysql</groupId>
      <artifactId>mysql-connector-j</artifactId>
    </dependency>

    <dependency>
      <groupId>org.projectlombok</groupId>
      <artifactId>lombok</artifactId>
    </dependency>
    <dependency>
      <groupId>org.junit.jupiter</groupId>
      <artifactId>junit-jupiter-api</artifactId>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>org.springframework.boot</groupId>
      <artifactId>spring-boot-test</artifactId>
      <version>${spring.boot.version}</version>
      <scope>test</scope>
    </dependency>

    <dependency>
      <groupId>org.springframework</groupId>
      <artifactId>spring-test</artifactId>
      <version>${spring.version}</version>
      <scope>test</scope>
    </dependency>

    <dependency>
      <groupId>com.microsoft.sqlserver</groupId>
      <artifactId>mssql-jdbc</artifactId>
    </dependency>

  </dependencies>

</project>
```

# 表结构

```sql
create table author
(
    id         integer primary key,
    first_name varchar(255),
    last_name  varchar(255),
    age        integer
);

create table article
(
    id          integer primary key,
    title       varchar(255) not null,
    description varchar(255),
    author_id   integer
);
```

## 数据库配置

```yaml
spring:
  datasource:
    url: jdbc:mysql:///nextentity?rewriteBatchedStatements=true
    username: root
    password: root
    driver-class-name: com.mysql.cj.jdbc.Driver
```

## 实体类

### src/main/java/io/github/nextentity/example/entity/Article.java

```java

@Data
public class Article {

    private Integer id;
    private String title;
    private String description;

    private Integer authorId;

    @ManyToOne
    @JoinColumn(name = "authorId")
    private Author author;

}
```

### src/main/java/io/github/nextentity/example/entity/Author.java

```java

@Data
public class Author {
    private Integer id;
    private String firstName;
    private String lastName;
    private Integer age;
}
```

## 测试用例

```java


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

```

package io.github.nextentity.example.entity;

import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import lombok.Data;

import java.util.Date;

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

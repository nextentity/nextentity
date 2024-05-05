package io.github.nextentity.example.entity;

import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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

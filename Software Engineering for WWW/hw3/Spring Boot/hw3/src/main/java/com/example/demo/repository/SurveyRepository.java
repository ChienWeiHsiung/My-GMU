/*
This is Repository
 */



package com.example.demo.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.example.demo.model.Survey;

@Repository
public interface SurveyRepository extends JpaRepository<Survey, Long>{

}

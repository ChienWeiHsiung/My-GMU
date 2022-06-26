/*
This is Controller ( for request mapping)
 */

package com.example.demo.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.demo.model.Survey;
import com.example.demo.repository.SurveyRepository;

@CrossOrigin(origins = "http://localhost:4200")
@RestController
@RequestMapping("/api")
public class SurveyController {
	
	@Autowired
	private SurveyRepository surveyRepository;
	
	//Get all Surveys
	@GetMapping("/surveys")
	public List<Survey> getAllSurveys(){
		return surveyRepository.findAll();
	}
	
	//Create survey rest api
	@PostMapping("/surveys")
	public Survey createSurvey(@RequestBody Survey survey) {
		return surveyRepository.save(survey);
	}
}

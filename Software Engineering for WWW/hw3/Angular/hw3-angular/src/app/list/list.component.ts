

import { Component, OnInit } from '@angular/core';
import { Survey } from '../survey';
//SurveyService is injected here
import { SurveyService } from '../survey.service';

@Component({
  selector: 'app-list',
  templateUrl: './list.component.html',
  styleUrls: ['./list.component.css']
})
export class ListComponent implements OnInit {

  surveys : Survey[] = [];

  constructor(private surveyService: SurveyService) { }

  ngOnInit(): void {
    this.getSurveys();
  }
  //get all surveys
  private getSurveys() {
    this.surveyService.getSurveyList().subscribe( (data) => {
      this.surveys = data;
    });
  }
}

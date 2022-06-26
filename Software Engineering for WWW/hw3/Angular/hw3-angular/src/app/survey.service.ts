/** Service : contains HttpClient and implements http get & post */



import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { Survey } from './survey';

@Injectable({
  providedIn: 'root'
})
export class SurveyService {

  baseURL: string = "http://localhost:8080/api/surveys";

  constructor(private httpClient: HttpClient) { }
  
  getSurveyList() : Observable<Survey[]>{
    return this.httpClient.get<Survey[]>(`${this.baseURL}`);
  }

  createSurvey(survey: Survey): Observable<any>{
    return this.httpClient.post(`${this.baseURL}`, survey);
  }
}

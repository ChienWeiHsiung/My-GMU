import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Survey } from '../survey';
import { SurveyService } from '../survey.service';

@Component({
  selector: 'app-survey',
  templateUrl: './survey.component.html',
  styleUrls: ['./survey.component.css']
})
export class SurveyComponent implements OnInit {
  //checkbox label and status
  box = [
    {
      label: 'Students',
      isChecked: false },
    {
      label: 'Location',
      isChecked: false },
    {
      label: 'Campus',
      isChecked: false },
    {
      label: 'Atmosphere',
      isChecked: false },
    {
      label: 'Dorm',
      isChecked: false },
    {
      label: 'Sports',
      isChecked: false }
  ]
  //store checked box labels
  checkedBoxs: string[] = [];
  //Survey instance
  survey: Survey = new Survey();
  constructor(private surveyService: SurveyService, private router: Router) { }

  ngOnInit(): void {
    this.fetchCheckedIDs()
  }
  //fetch checked box labels
  fetchCheckedIDs() {
    //initialize
    this.checkedBoxs = [];
    this.box.forEach((value, index) => {
      if (value.isChecked) {
        this.checkedBoxs.push(value.label);
      }
    });
  }
  //save survey instance
  saveSurvey(){
    this.surveyService.createSurvey(this.survey).subscribe( data => {
      console.log(data);
    },
    error => console.log(error));
  }
  
  //Submit function
  onClickSubmit(data: any){
    this.fetchCheckedIDs();
    //Assign data to instance
    this.survey.firstName = data.fname;
    this.survey.lastName = data.lname;
    this.survey.street = data.Street;
    this.survey.city = data.City;
    this.survey.state = data.State;
    this.survey.zip = data.Zip;
    this.survey.phone = data.Phone;
    this.survey.email = data.Email;
    this.survey.date = data.Date;
    if (  this.checkedBoxs.length == 0  ){
      this.survey.checkbox = "Empty"
    }else{
      this.survey.checkbox = this.checkedBoxs.toString();
    }
    if (  data.ask2 == 0  ){
      this.survey.radio = "Empty"
    }else{
      this.survey.radio = data.ask2;
    }
    if (  data.Comments == 0  ){
      this.survey.comments = "Empty"
    }else{
      this.survey.comments = data.Comments;
    }
    if (  data.rating == 0  ){
      this.survey.rating = "Empty"
    }else{
      this.survey.rating = data.rating;
    }
    console.log(this.survey);
    //save instance
    this.saveSurvey();
    alert("Submit Successfully!!");
    //routing to home page
    this.router.navigate(['/', 'homepage']);
  }

}

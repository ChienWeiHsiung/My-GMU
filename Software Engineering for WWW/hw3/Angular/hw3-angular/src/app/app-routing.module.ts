/** Routing between my components */

import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

//my component
import { SurveyComponent } from './survey/survey.component';
import { ListComponent } from './list/list.component';
import { HomepageComponent } from './homepage/homepage.component';

const routes: Routes = [
  {path:'survey', component: SurveyComponent },
  {path:'list', component: ListComponent },
  {path:'homepage', component: HomepageComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
export const routingComponents = [SurveyComponent, ListComponent, HomepageComponent]

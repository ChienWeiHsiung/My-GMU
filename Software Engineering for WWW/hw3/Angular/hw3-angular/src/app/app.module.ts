import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

//add routingComponents ( including my components )
import { AppRoutingModule, routingComponents} from './app-routing.module';
import { AppComponent } from './app.component';
//Forms
import { FormsModule } from '@angular/forms';
//Http Client
import { HttpClientModule } from '@angular/common/http';

@NgModule({
  declarations: [
    AppComponent,
    routingComponents
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule,
    HttpClientModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }



function Greeting() {
    var now = new Date();
    var hour = now.getHours();
    var name;
    var message;
    
    if ( hour < 12 )
        message = "Good Morning, ";
    else{
        hour = hour - 12;
        if (hour < 6 )
            message = "Good Afternoon, ";
        else
            message = "Good Evening, ";
    }
    
    if ( document.cookie ){
        var myCookie = decodeURI(document.cookie); 
        var cookieTokens = myCookie.split("=");
        name = cookieTokens[1];
        message +=  name + ", welcome to SWE642 Survey";
        //Show hyperlink
        var hyperlink = document.getElementById("hyperlink");
        hyperlink.text = "Click here if you're NOT " + name + "!";
        hyperlink.style.display = "inline";
    }
    else{
        name = window.prompt("Please enter your name", "your name");
        if ( !name ){
            name = "anonymous ";
            message +=  name + ", welcome to SWE642 Survey";
            document.cookie = "name=" + encodeURI("anonymous");
        }else{
            message +=  name + ", welcome to SWE642 Survey";
            document.cookie = "name=" + encodeURI(name);
        }
        //document.cookie = "name=" + encodeURI(name);
    }
    document.getElementById("greeting-text").innerText = message;
    dialogDisplay();
}


function dialogDisplay(){
    //Get the dialog (div)
    var modal = document.getElementById("greeting");
    //Get the <span> element that closes the dialog
    var span = document.getElementById("close");
    //Open the dialog
    modal.style.display = "block";
    //When click on <span> (x), close the dialog
    span.onclick = function() {
        modal.style.display = "none";
    }
     //When click anywhere outside of the dialog, close it
    window.onclick = function(event) {
        if (event.target == modal) {
          modal.style.display = "none";
        }
    }
}

function wrongPerson(){
    var now = new Date();
    var time = now.getTime();
    var expireTime = time + 100; //+1sec
    now.setTime(expireTime);
    document.cookie = "name=null;expires="+ now.toUTCString();
    location.reload();
}

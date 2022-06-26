
function zipcode(){
    $("#status").text("checking zip...");
    $("#status").css("color", "red");
    //document.getElementById("status").innerHTML = "checking zip";
    $.ajax({
        url: "zipcode.json",
        dataType: "json",
        success: function(text){
            parseZip(text);
        }
  });
}

function parseZip(text){
    //input zip code
    var zip = $("#Zip").val();
    //empty check
    if ( !zip ){
        $("#City").text("City : ");
        $("#State").text("State : ");
        $("#status").text("Empty");
        $("#status").css("color", "red");
        return;
    }
    //json data length
    var data_length = Object.keys(text.zipcodes).length;
    //loop for searching
    for ( let i = 0 ; i < data_length ; i ++ ){
        if ( zip == text.zipcodes[i]['zip'] ){
            //label city, state
            $("#City").text("City : " + text.zipcodes[i]['city']);
            $("#State").text("State : " + text.zipcodes[i]['state']);
            //checking info
            $("#status").text("valid");
            $("#status").css("color", "blue");
            return;
        }
    }
    //checking info
    $("#City").text("City : ");
    $("#State").text("State : ");
    $("#status").text("An invalid zip");
    $("#status").css("color", "red");

}

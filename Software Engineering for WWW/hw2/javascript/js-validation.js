

function validation() {
    
    var output = "";
    //0 : ok, 1 : error
    var flag = 0;
    //name
    var fname = $("#fname").val();
    var lname = $("#lname").val();
    var nameRegex = /^[A-Za-z]+$/;
    if ( !fname || !lname ){
        //empty input
        flag += 1;
        output += flag + ". Name can't be blank.\n";
    }else if ( !(fname.match(nameRegex) && lname.match(nameRegex)) ){
        //error content
        flag += 1;
        output += flag + ". Name should contain only alphabets.\n";
    }
    //street address
    var address = $("#Street").val();
    var addressRegex = /^[a-zA-Z0-9\s,.'-]*$/;
    if ( !address ){
        //empty input
        flag += 1;
        output += flag + ". Street Address can't be blank.\n";
    }else if ( !(address.match(addressRegex)) ){
        //error content
        flag += 1;
        output += flag + ". Street Address should contain only alphabets, numeric, space and ,.'- .\n";
    }
    //zipcode
    var zipcode = $("#Zip").val();
    var zipcodeRegex = /^[0-9\s,.'-]*$/;
    if ( !zipcode ){
        //empty input
        flag += 1;
        output += flag + ". Zipcode can't be blank.\n";
    }else if ( !(zipcode.match(zipcodeRegex)) ){
        //error content
        flag += 1;
        output += flag + ". Zipcode should contain only numeric.\n";
    }else if ( $("#status").text() != "valid"){
        flag += 1;
        output += flag + ". Zipcode is invalid.\n";
    }
    //E-mail
    var mailformat = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;
    var mail = $("#Email").val();
    if ( mail == "" ){
        //empty input
        flag += 1;
        output += flag + ". Email can't be blank.\n";
    }else if ( !(mail.match(mailformat)) ){
        //error content
        flag += 1;
        output += flag + ". The format of Email is wrong.\n";
    }
    //checkbox
    var num_checked = $('input:checkbox:checked').length;
    if ( num_checked < 2){
        flag += 1;
        output += flag + ". At least two checkboxes should be checked.\n";
    }
    //radio
    var radio_checked = $('input:radio:checked').length;
    if ( radio_checked != 1){
        flag += 1;
        output += flag + ". A radio button option should be selected.\n";
    }
    if ( flag >= 1){
        var dialogBlock = document.getElementById("dialog-text");
        dialogBlock.innerText = output;
        dialogBlock.style.fontWeight = "850";
        $( "#dialog" ).dialog({
            title: "Input Errors",
            width: 700,
            autoResize : true
        });
        return false;
    }else{
        $("#dialog").dialog('close');
        return true;
        //document.getElementById("survey").submit();
    }
}

function clearForm() {
    document.getElementById("survey").reset();
 }


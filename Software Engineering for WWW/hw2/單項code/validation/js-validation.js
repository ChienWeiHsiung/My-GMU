

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
        output += "1. Name can't be blank.\n";
        flag = 1;
    }else if ( !(fname.match(nameRegex) && lname.match(nameRegex)) ){
        //error content
        output += "1. Name should contain only alphabets.\n";
        flag = 1;
    }else{
        output += "1. Name : OK.\n";
    }
    //address
    var address = $("#Street").val();
    var addressRegex = /^[a-zA-Z0-9\s,.'-]*$/;
    if ( !address ){
        //empty input
        output += "2. Address can't be blank.\n";
        flag = 1;
    }else if ( !(address.match(addressRegex)) ){
        //error content
        output += "2. Address should contain only alphabets, numbers, space and ,.'- .\n";
        flag = 1;
    }else{
        output += "2. Address : OK.\n";
    }
    //E-mail
    var mailformat = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;
    var mail = $("#Email").val();
    if ( mail == "" ){
        //empty input
        output += "3. Email can't be blank.\n";
        flag = 1;
    }else if ( !(mail.match(mailformat)) ){
        //error content
        output += "3. The format of Email is wrong.\n";
        flag = 1;
    }else{
        output += "3. Email : OK.\n";
    }
    //checkbox
    var num_checked = $('input:checkbox:checked').length;
    if ( num_checked < 2){
        output += "4. At least two checkboxes should be checked.\n";
        flag = 1;
    }else{
        output += "4. Checkbox : OK.\n";
    }
    //radio
    var radio_checked = $('input:radio:checked').length;
    if ( radio_checked != 1){
        output += "5. A radio button option should be selected.\n";
        flag = 1;
    }else{
        output += "5. Radio : OK.\n";
    }
    if ( flag == 1){
        var dialogBlock = document.getElementById("dialog-text");
        dialogBlock.innerText = output;
        dialogBlock.style.fontWeight = "850";
        $( "#dialog" ).dialog({
            title: "Some Input Errors",
            width: 700,
            height: 250
        });
    }else{
        //form的name
        document.getElementById("survey").submit();
    }
}

function clearForm() {
    document.getElementById("survey").reset();
 }


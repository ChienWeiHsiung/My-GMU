function rollClick() {

    //if Raffle's input is valid
    if ( $("#check").text() == "Validation." ){
        var st = "Your numbers : ";
        //Get 10 random numbers
        var numlist = Array.from({length: 100}, (_, i) => i + 1);
        const shuffled = numlist.sort((a, b) => 0.5 - Math.random());
        var rafflo = shuffled.splice(0, 10);

        //get input and trim the space
        var input = $("#data").val().trim();
        //split by comma
        var nums = input.split(',');

        var count = 0;
        for (i = 0 ; i < 10 ; i ++){
            for (j = 0 ; j < 10 ; j ++){
                if ( nums[i] == rafflo[j] )
                    count ++;
            } 
        }

        var price = "";
        if (count >= 1){
            price = "Congratulation!!!!"
        }else{
            price = "Unfortunately~~~"
        }
        alert(st + nums + "\n" + "Rafflo numbers : " + rafflo + "\n" + price);
        return true;
    }else{
        //Raffle's input is not valid 
        alert("Raffle's input is not valid.");
        return false;
    }
    
}

function checkInput(){
    //get input and trim the space
    var input = $("#data").val().trim();
    //split by comma
    var nums = input.split(',');
    var intnums = [];
    //check whether a comma left at the end
    if (input[input.length-1] == ','){
        $("#check").text("A comma left at the end.");
        $("#check").css("color", "red");
        return;
    }
    //check 10 numbers
    if ( nums.length < 10 ){
        $("#check").text("Please input 10 numbers.");
        $("#check").css("color", "red");
        return;
    }
    if ( nums.length > 10 ){
        $("#check").text("Please input 10 numbers.");
        $("#check").css("color", "red");
        return;
    }
    //chekc inputs are only numbers
    for (let i = 0 ; i < nums.length ; i ++){
        let temp = nums[i].trim();
        if ( isNum(temp) ){
            intnums[i] = parseInt(temp);
        }else{
            $("#check").text("Please input numbers and comma only.");
            $("#check").css("color", "red");
            return;
        }
    }
    //check range from 1 to 100
    for (let i = 0 ; i < intnums.length ; i ++){
        if ( intnums[i] > 100 || intnums[i] < 1 ){
            $("#check").text("Please input Numbers ranging from 1 to 100.");
            $("#check").css("color", "red");
            return;
        }
    }
    $("#check").text("Validation.");
    $("#check").css("color", "blue");
    return;
}

function isNum(val){
    return !isNaN(val)
}

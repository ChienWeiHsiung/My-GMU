
function dataProcess(){
    var nums = checkInput();
    compute(nums);
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
    //check at least 10 numbers
    if ( nums.length < 10 ){
        $("#check").text("Please input at least 10 numbers.");
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
    return intnums;
}
function compute(nums){
    let sum = 0;
    for (let i = 0 ; i < nums.length ; i ++){
        sum += nums[i];
    }
    var max = Math.max.apply(null, nums);
    const average = (array) => array.reduce((a, b) => a + b) / array.length;
    var avg = average(nums);
    $("#maximum").text(max);
    $("#average").text(avg);

}

function isNum(val){
    return !isNaN(val)
}
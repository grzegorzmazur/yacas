function square_root_parser( outputValue, parameters ){
    console.log( "This is squre root parser");
    return "Sqrt(" + outputValue + ")";
}



function natural_logarithm_parser( outputValue, parameters ){
    console.log( "This is natural logarithm parser");
    return "Ln(" + outputValue + ")";
}

function exponential_parser( outputValue, parameters ){
    console.log( "This is exponential parser");
    return "Exp(" + outputValue + ")";
}


function simplify_parser( outputValue, parameters ){
    console.log( "This is simplify parser");
    return "Simplify(" + outputValue + ")";
}

function integrate_parser( outputValue, parameters ){
    console.log( "This is integrate parser");
    variable = parameters["variable"];
    
    definite = parameters["definite"];
    
    if ( definite ){
        to = parameters["to"];
        from = parameters["from"];
        return "Integrate("+ variable + "," + from + "," + to + ")" + outputValue;
    }else{
        return "Integrate("+ variable +")" + outputValue;
    }
}

function derivative_parser( outputValue, parameters ){
    console.log( "This is derivative parser");
    variable = parameters["variable"];
    return "D("+ variable +")" + outputValue;
}

function plot_2d_parser( outputValue, parameters ){
    console.log( "This is Plot 2D parser");
    
    from = parameters["from"];
    to = parameters["to"];
    
    
    return "Plot2D(" + outputValue + "," + from + ":" + to + ")";
}

function plot_3d_parser( outputValue, parameters ){
    console.log( "This is Plot 3D parser");
    
    from_0 = parameters["variable_0_from"];
    to_0 = parameters["variable_0_to"];
    from_1 = parameters["variable_1_from"];
    to_1 = parameters["variable_1_to"];

    return "Plot3DS(" + outputValue + "," + from_0 + ":" + to_0 + "," + from_1 + ":" + to_1 + ")";
}

function limit_parser( outputValue, parameters ){
    console.log( "This is limit parser");
    
    variable = parameters["variable"];
    value = parameters["value"];
    direction = parameters["direction"];


    if ( direction ){
        from = parameters["from"];
        return "Limit(" + variable + "," + value + "," + from + ")" + outputValue;

    }else{
        return "Limit(" + variable + "," + value + ")" + outputValue;

    }
}


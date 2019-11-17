"use strict";

function square_root_parser( outputValue, parameters ){
    return "Sqrt(" + outputValue + ")";
}



function natural_logarithm_parser( outputValue, parameters ){
    return "Ln(" + outputValue + ")";
}

function NumericalValueParser( outputValue, parameters ){
    const with_precision = parameters["with_precision"];

    if ( with_precision ){
        const precision = parameters["precision"];
        return "N("+ outputValue + "," + precision + ")";
    }else{
        return "N(" + outputValue + ")";
    }
}

function ExponentialParser( outputValue, parameters ){
    return "Exp(" + outputValue + ")";
}


function SimplifyParser( outputValue, parameters ){
    return "Simplify(" + outputValue + ")";
}

function IntegrateParser( outputValue, parameters ){
    const variable = parameters["variable"];

    const definite = parameters["definite"];

    if ( definite ){
        const to = parameters["to"];
        const from = parameters["from"];
        return "Integrate("+ variable + "," + from + "," + to + ")" + outputValue;
    }else{
        return "Integrate("+ variable +")" + outputValue;
    }
}

function DerivativeParser( outputValue, parameters ){
    const variable = parameters["variable"];
    return "D("+ variable +")" + outputValue;
}

function Plot2dParser( outputValue, parameters ){
    const from = parameters["from"];
    const to = parameters["to"];

    return "Plot2D(" + outputValue + "," + from + ":" + to + ")";
}

function Plot3dParser( outputValue, parameters ){
    const from_0 = parameters["variable_0_from"];
    const to_0 = parameters["variable_0_to"];
    const from_1 = parameters["variable_1_from"];
    const to_1 = parameters["variable_1_to"];

    return "Plot3DS(" + outputValue + "," + from_0 + ":" + to_0 + "," + from_1 + ":" + to_1 + ")";
}

function LimitParser( outputValue, parameters ){
    const variable = parameters["variable"];
    const value = parameters["value"];
    const direction = parameters["direction"];


    if ( direction ){
        const from = parameters["from"];
        return "Limit(" + variable + "," + value + "," + from + ")" + outputValue;

    }else{
        return "Limit(" + variable + "," + value + ")" + outputValue;

    }
}


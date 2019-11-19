"use strict";

MathBar.selectMoreText = "more";
MathBar.functions;
MathBar.categories;

//VIF - Very Importatnt Function
function MathBar( outputID, options, callback ) {
    let self = this;

    let $button = $("<button>", {name: outputID, class: "MathBarButton"});
    $button.click( function(){ self.Toggle(); });
    $( "#" + outputID ).append( $("<div>", {class: "inside"}).append( $button ));
    self.button = $button;

    if (options["layout"] === undefined )
        self.layout = "singleline";
    else
        self.layout = options["layout"];

    self.categories = MathBar.categories[ options["type"] ];

    if (options["VIF"] === undefined || options["VIF"] === "max" )
        self.numberOfVIF = self.categories.length;
    else
        self.numberOfVIF = options["VIF"];

    self.outputID = outputID;
    self.defaultParameters = options["defaultParameters"];
    self.callback = callback;

    self.drawn = false;
    self.currentOption = "";
    self.currentOptionVIF = true;
    self.visible = false;
    self.outputValue = $("#" + outputID)[0].yacasExpression;

    $button.on("keydown", function( event ){
                 if( event.which === 13  ){
                    event.preventDefault();
                    if ( event.shiftKey )
                        this.mathBar.Run();
                }
              });
}

MathBar.prototype.drawMathBar = function(){
    let $functionsDiv = this.createFunctionsDiv();
    let $submitButton = this.createSubmitButton();
    let $mathBarTable = this.createMathBarTable();

    $mathBarTable.find(".functions").append( $functionsDiv );
    $mathBarTable.find(".submitButton").append( $submitButton );

    let $mathBarElement = $( "<div>" , { class: "MathBar" } ).hide();

    $( "#" + this.outputID ).after( $mathBarElement.append( $mathBarTable ));

    this.mathBarElement = $mathBarElement;
    $mathBarElement[0].mathBar = this;
    this.Show();

    //In case number of all functions is 1 more than VIF do not display combobox with only one function
    //but treat all of them as VIFs
    if ( this.layout !== "multiline")
        if ( this.numberOfVIF === this.categories.length )
            $functionsDiv.parent().width( $functionsDiv.width() + 1 );
        else
            $functionsDiv.parent().width( $functionsDiv.width()  );

    this.optionClicked( this.categories[0], true );

    let $functionsSelect = $functionsDiv.find("select");
    if ( $functionsSelect ){
        $functionsSelect.selectmenu();
        $functionsSelect.on( "selectmenuselect", function( event, ui ) {
                            $mathBarElement = $(this).parents(".MathBar:first");
                            $mathBarElement[0].mathBar.optionClicked( $("option:selected", this).attr("name"), false ); } );
    }
}

MathBar.prototype.createSubmitButton = function(){
    let $submitButton = $("<button>", {class: "submitButton"} );
    $submitButton.click( function(){
                        let $mathBarElement = $(this).parents(".MathBar:first");
                        $mathBarElement[0].mathBar.Run(); });
    return $submitButton;
}

MathBar.prototype.createFunctionsDiv = function(){
    if ( this.numberOfVIF > this.categories.length )
        this.numberOfVIF = this.categories.length;

    if ( this.categories.length - this.numberOfVIF === 1 )
        this.numberOfVIF++;

    let $functionsDiv = $("<div>", {class : "radio_group_horizontal styled_radio"});
    let i;
    for( i = 0; i < this.numberOfVIF; i++){

        let func = MathBar.functions[this.categories[i]];
        let text = func["text"];

        let $input = $("<input>", { type: "radio", name: this.outputID, value: this.categories[i]});

        if ( i === 0 )
            $input.prop( "checked", true );

        $input.click( function(){
                     let $mathBarElement = $(this).parents(".MathBar:first");
                     $mathBarElement[0].mathBar.optionClicked( this.value, true );
                     });

        let $span = $("<span>").append( text );
        let $label = $("<label>");
        $label.append( $input ).append( $span );

        $functionsDiv.append( $label );
        $label.dblclick( function(e){ e.stopPropagation(); this.Run(); });
    }

    if ( i !== this.categories.length ){

        let $functionsSelect = $("<select>");
        let $option = $("<option>").append( MathBar.selectMoreText );
        $option.attr( "disabled", true );
        $option.attr( "selected", true);
        $functionsSelect.append($option );

        for( let j = this.numberOfVIF; j < this.categories.length; j++){
            let func = MathBar.functions[this.categories[j]];
            let text = func["text"];
            $functionsSelect.append( $("<option>", {name: this.categories[j]}).append( text ) );
        }

        let $label = $("<label>");
        $label.dblclick( function(e){
                        if ( $label.find("option:selected").val() === MathBar.selectMoreText ){
                        return true;
                        }
                        e.stopPropagation();
                        this.Run();
                        return false;
                        });
        $functionsDiv.append($label.append($functionsSelect ));
    }

    return $functionsDiv;
}

MathBar.prototype.createMathBarTable = function(){

    if (this.layout === "singleline"){

        let $mathRow = $("<tr>");

        $mathRow.append( $("<td>", {class: "functions"}));
        $mathRow.append( $("<td>", {class: "separator"}));
        $mathRow.append( $("<td>", {class: "parameters"}).append( $("<div>", {class: "parameters"})));
        $mathRow.append( $("<td>", {class: "submitButton"}));

        return $("<table>").append( $mathRow );
    }

    if ( this.layout === "multiline"){
        let $functionsRow = $("<tr>");

        $functionsRow.append( $("<td>", {class: "functions"}));
        $functionsRow.append( $("<td>", {class: "submitButton"}));

        let $parametersRow = $("<tr>");
        $parametersRow.append( $("<td>", {class: "parameters", colspan:"2"}).append( $("<div>", {class: "parameters"})));

        return $("<table>").append( $functionsRow ).append( $parametersRow );
    }
}


MathBar.keydownEventHandler = function( event ){
    let $mathBarElement = $(this).parents(".MathBar:first");
    if( event.which === 13 && event.shiftKey ){
        event.preventDefault();
        $mathBarElement[0].mathBar.Run();
        return false;
    }
    return true;
};


MathBar.ParseFunctions = function(){
    let keys = Object.keys( MathBar.categories );
    for ( let j = 0; j < keys.length; j++ ){
        let funcList = MathBar.categories[ keys[j] ];
        for (let i = 0; i < funcList.length; i++){
            let ff = MathBar.functions[ funcList[i] ];
            if ( ff == undefined )
                console.error( "Function " + funcList[i] + " is not defined!");
        }
    }
    keys = Object.keys( MathBar.functions );
    for ( let j = 0; j < keys.length; j++ ){
        let func = MathBar.functions[ keys[j]];
        if ( func["text"] == undefined )
            func["text"] = keys[j];
        let parameters = func["parameters"];
        if ( parameters == undefined )
            func["parameters"] = [];

        for ( let i = 0; i < parameters.length; i++ )
            MathBar.ParseParameter( parameters[i] );

        if ( func["parser"] == undefined )
            console.error( "Parser for function: " + keys[j] + " is undefined");
        else
            if( window[func["parser"]] == undefined )
                console.error( "Parser: " + func["parser"] + " is undefined");
    }
}

MathBar.ParseParameter = function( parameter ){
    if ( parameter["parameterName"] == undefined ){
        console.error( "Name of parameter is undefined: " + parameter);
        parameter["parameterName"] = "undefined";
    }

    if ( parameter["text"] == undefined )
        parameter["text"] = parameter["parameterName"];

    if ( parameter["parameterType"] == undefined ){
        parameter["parameterType"] = "edit";
        console.warning( "Lack of type of parameter: " + parameter["parameterName"] );
    }

    if ( parameter["defaultValue"] == undefined )
        switch( parameter["parameterType"] ){
            case "label":
                parameter["defaultValue"] = "";
                break;
            case "edit":
                parameter["defaultValue"] = "";
                break;
            case "checkbox":
                parameter["defaultValue"] = false;
                break;
            case "condition":
                parameter["defaultValue"] = false;
                break;
            case "select":
                parameter["defaultValue"] = [];
                break;
            default:
                console.error( "This paramter type is not implemented: "+ type + "!");
        }

    if ( parameter["parameterType"] === "edit" )
        if ( parameter["widestValue"] != undefined )
            parameter["inputWidth"] = MathBar.calculateInputWidth( parameter["widestValue"] );
        else
            parameter["inputWidth"] = MathBar.calculateInputWidth( parameter["defaultValue"] ) * 3;

    let parameters = parameter["parameters"];
    if ( parameters != undefined )
        for ( let i = 0; i < parameters.length; i++ )
            MathBar.ParseParameter( parameters[i] );
}

MathBar.prototype.optionClicked = function( functionName, VIF ){

    if ( this.currentOptionVIF && functionName === MathBar.selectMoreText )
        return;
    this.currentOptionVIF = VIF;
    this.currentOption = functionName;


    let $parametersElement = $(this.mathBarElement).find(".parameters");
    $parametersElement.html("");

    if ( functionName === MathBar.selectMoreText )
        return;

    if ( !VIF ){
        $(this.mathBarElement).find( "input:checked" ).prop("checked", false);
        $(this.mathBarElement).find( "select").parent().addClass("checked");
    }else{
        $(this.mathBarElement).find( "select").parent().removeClass("checked");
    }

    let func = MathBar.functions[ functionName ];
    let parameters = func["parameters"];

    for ( let i = 0; i < parameters.length; i++ )
        $parametersElement.append( this.getPropertyLabel( parameters[i]) );

    $parametersElement.find("select").selectmenu();
};

MathBar.prototype.changeConstToVariables = function ( text ){

    let variables = this.defaultParameters["variable"];

    if (variables == undefined )
        return text;

    if ( $.isArray( text ) ){
        for ( let k = 0; k < text.length; k++ )
            text[k] = this.changeConstToVariables( text[k]);
        return text;
    }

    if ( variables.length === 1 )
        text = text.replace("%VARIABLE%", variables );

    if ( variables.length > 1 )
        for ( let j = 0; j < variables.length; j++ ){
            textToSearch = "%VARIABLE%" + j + "%";
            text = text.replace( textToSearch , variables[j] );
        }

    return text;
}

MathBar.prototype.getPropertyLabel = function( parameter ){
    let $label = $("<label>");
    let type = parameter["parameterType"];

    let value = undefined;
    if ( this.defaultParameters != undefined )
        value = this.defaultParameters[parameter["parameterName"]];

    if( value == undefined )
        value = parameter["defaultValue"];

    let text = parameter["text"];

    if ( this.defaultParameters != undefined ){
        value = this.changeConstToVariables( value );
        text = this.changeConstToVariables( text );
    }

    if ( type === "select" ){
        if ( value.length === 1 ){
            type = "label";
            value = value[0] ;
        }
    }

    if ( type === "label"){
        let $span = $("<span>", {class: "parameter", name: parameter["parameterName"]});
        $span.append( value );

        if ( text === "" ){
            $span.css("margin-left", "0px");
        }else{
            $label.append( text );
        }

        $label.append( $span );
    }

    if ( type === "edit"){
        let $input = $("<input>", {type: "text", name: parameter["parameterName"]});
        $input.keydown( MathBar.keydownEventHandler );
        $input.css( "width", parameter["inputWidth"]);
        $input.val( value );

        if ( text !== "" ){
            $label.append( text );
        }

        $label.append( $input );
    }

    if ( type === "checkbox"){
        let $input = $("<input>", {type: "checkbox", name: parameter["parameterName"] });
        $input.prop( "checked", value);
        $label.append( $input ).append( text);
    }

    if ( type === "condition"){
        let $input = $("<input>", {type: "checkbox", name: parameter["parameterName"] });

        let checked = value === "true" ;

        $input.change( function(){
                      $mathBarElement = $(this).parents(".MathBar:first");
                      MathBar.toggleParameters( $mathBarElement, parameter["parameterName"], !$(this).is(":checked"));
                    });

        $input.prop( "checked", checked );
        $label.append( $input ).append( text );

        let conditionalParameters = parameter["parameters"];
        let $outerlabel = $("<span>").append( $label );

        for ( let i = 0; i < conditionalParameters.length; i++ ){
            let $condparLabel = this.getPropertyLabel( conditionalParameters[i]);
            $condparLabel.addClass( "check_" + parameter["parameterName"]);

            if ( !checked ){
                $condparLabel.addClass( "labelDisabled" );
                $condparLabel.find("input").prop( "disabled", true );
                $condparLabel.find("select").prop( "disabled", true );
            }

            $outerlabel.append( $condparLabel );
        }

        $label = $outerlabel;
    }

    if ( type === "select"){

        let $select = $("<select>", { name: parameter["parameterName"] });

        for( let i = 0; i < value.length; i++){
            $select.append( $("<option>").append( value[i]) );
        }

        $label.append( text ).append( $select );
    }

    return $label;
};

MathBar.toggleParameters = function( $mathBarElement, parameterName, disabled ){
    $label =  $mathBarElement.find(".check_" + parameterName ).toggleClass( "labelDisabled", disabled );
    $label.find("input").prop( "disabled", disabled );
    if ( disabled )
        $label.find("select").selectmenu( "disable" );
    else
        $label.find("select").selectmenu( "enable" );
}

MathBar.prototype.GetPropertyValue = function( parameter, outValues ){
    let type = parameter["parameterType"];
    let parameterName = parameter["parameterName"];

    let $element = this.mathBarElement.find( "[name='" + parameterName + "']:first" );

    switch( type ){
        case "label":
            outValues[ parameterName ] = $element.text();
            break;
        case "edit":
            outValues[ parameterName ] = $element.val();
            break;
        case "checkbox":
            outValues[ parameterName ] = $element.prop("checked");
            break;
        case "condition":
            outValues[ parameterName ] = $element.prop("checked");

            let conditionalParameters = parameter["parameters"];
            for ( let i = 0; i < conditionalParameters.length; i++ ){
                this.GetPropertyValue( conditionalParameters[i], outValues );
            }
            break;
        case "select":
            if ( $element.val() !== "") outValues[ parameterName ] = $element.val();
            else outValues[ parameterName ] = $element.text();
            break;
        default:
            console.error( "This paramter type is not implemented: "+ type + "!");
    }
}

MathBar.prototype.Run = function(){

    let func = MathBar.functions[this.currentOption];

    let parameters = func["parameters"];
    let outValues = [];

    for ( let i = 0; i < parameters.length; i++ )
        this.GetPropertyValue( parameters[i], outValues );

    let parser = func["parser"];
    let result = window[parser](this.outputValue, outValues);

    this.callback( result, this.outputID );
    this.Hide();
}

MathBar.prototype.Remove = function(){
    this.button.mathBar = null;
    this.mathBarElement.slideUp(300, function(){ this.remove() ;} );
    $(this.button).removeClass("up");
}

MathBar.prototype.Show = function(){
    this.visible = true;
    this.mathBarElement.slideDown(300);
    $(this.button).addClass("up");
}

MathBar.prototype.Hide = function(){
    this.visible = false;
    this.mathBarElement.slideUp(300);
    $(this.button).removeClass("up");
}

MathBar.prototype.Toggle = function(){

    if ( this.visible )
        this.Hide();
    else
        if ( !this.drawn ) this.drawMathBar();
            this.Show();
}


MathBar.initializeFunctions = function(jsonfile){
    let xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState === 4 ){
            if (xhttp.status === 200 || xhttp.status === 0){
                const data =  jQuery.parseJSON(xhttp.responseText);
                MathBar.functions = data["functions"];
                MathBar.categories = data["categories"];
                MathBar.ParseFunctions();
            }else{
                console.error( "Couldn't load json file");
            }
        }
    }

    xhttp.open("GET", jsonfile, true);
    xhttp.send();
}

MathBar.supportsExpressionType = function( expressionType, numberOfVariables ){
    if ( numberOfVariables > 0 )
        expressionType += "_" + numberOfVariables;

    if( MathBar.categories[expressionType] != undefined )
        return true;

    return false;
}

MathBar.calculateInputWidth = function( value ){
    let $testDiv = $("#MathBarStringWidthTest");
    $testDiv.show();
    $testDiv.text( value );
    let width = $testDiv.width()+1;
    $testDiv.hide();
    return width;
}

var bluredEditable = null;


function editableLoad(){
    $( window ).on( 'blur', function(){ if ( bluredEditable !== null ) $( bluredEditable ).click(); } );
    $( document ).on( 'click', function(){ bluredEditable = null; });
}

function editableReset( element ){
    original = $(element).parents("span")[0].calculatedExpression;
    revert = $(element).parents("span")[0].revert;
    if ( original === revert ){
        $(element).parents("tbody").removeClass("Modified");
    }
}

function editableBlur( element ){
    bluredEditable = element;
    $(element).parents("tbody").addClass("NotToCalculate");
    $(element).children().submit();
}

var EmptyEditableText = "Click to edit...";

function changeToEditable( elementID, number ){
    $( elementID ).editable(
                            function(value, settings) { return value; },
                            {
                            width   : '100%',
                            type    : "autogrow",
                            tooltip : EmptyEditableText,
                            style   : "width:100%",
                            onreset: function(value,settings){
                            editableReset( this );
                            },
                            onblur  : function( value, setting){
                            editableBlur ( this );
                            },
                            name    : number,
                            callback: function( value, settings ){
                            processChange( value, settings.name, this  );
                            }
                            });
}


function addEditable( lineid, number, value, rootElementID ){
    
    var row = $( "<tr class='In'></tr>" );
    row.append( "<td>in&nbsp&nbsp"+ number + ":</td>" );
    row.append( "<td><span class='editable'>"+ value +"</span></td>" );
    $( rootElementID ).append( row );
    
    var editable = row.find(".editable");
    editable[0].calculatedExpression = value;
    
    changeToEditable( editable, lineid );
    return editable;
}

function evaluateCurrent_deprecatedEditable(){
    var active = document.activeElement;
    if ( active.id === "inputExpression" && active.value != ""){
        calculate( active.value );
    }else{
        $(document.activeElement).parent().trigger("submit");
    }
}

function evaluateAll_deprecatedEditable(){
    $(".editable").each( function() {
                        value = $(this).text();
                        
                        if ( value === "" ){
                        $(this).find("form:first").trigger("submit");
                        }else{
                        
                        if ( value === EmptyEditableText ) value = "";
                        
                        number = $(this).parents("tbody")[0].id.split("_")[1];
                        processChange( value, number, this );
                        }
                        });
    inputVal = $( "#inputExpression" ).val();
    if ( inputVal !== "" ) calculate( inputVal );
    $("#inputExpression").focus();
}

function getAllInputs_depreciateEditable(){
    var inputs = [];
    $(".editable").each( function() {
                        value = $(this).text();
                        
                        if ( value === "" ){
                        inputs.push( $(this).find("textarea:first").val() );
                        }else{
                        
                        if ( value === EmptyEditableText ) value = "";
                        inputs.push( value );
                        }
                        });
    inputVal = $( "#inputExpression" ).val();
    if ( inputVal !== "" ) inputs.push( inputVal );
    
    return inputs;
}
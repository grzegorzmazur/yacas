/* global CodeMirror, MathBar, MathJax, THREE, vis, yacas */

//Function stops scrolling event, and just scrool the window
//Otherwise Code-Mirror editor kept scrolling one line up even if there was no line to scroll
function scrollListener(e){

    dx = e.wheelDeltaX;
    dy = -1 * e.wheelDeltaY;
    
    window.scrollBy( dx, dy);
    e.preventDefault();
}


function load(){
    
    MathBar.initializeFunctions( "mathbar/functions.json" );

    //To keep CodeMirror editor from bad scrolling
    document.body.addEventListener( "mousewheel", function(e){ scrollListener(e);} );
    
    $( "#inputExpression" ).autosize();

    $( window ).on( 'resize', function( event ){
        
        w = $("body").width() - $("#Elements>tbody>tr>td:first-child").innerWidth() - 10; //10 is for padding in the td.Out element
        $( ".resizable" ).each( function(){
                               maxwidth = $(this).resizable( "option", "maxWidth");
                               if ($(this).width() > w || $(this).width() == maxwidth ) $(this).width(w);
                               });
        $( ".resizable" ).resizable( "option", "maxWidth", w );
       // $( ".CodeMirror").width( w - 2 );
    });
    
    CodeMirror.defaults['lineNumbers'] = false;
    CodeMirror.defaults['mode'] = {name: 'yacas'};
    CodeMirror.defaults['matchBrackets'] = true;
    CodeMirror.defaults['autoCloseBrackets'] = '()[]{}""';
    CodeMirror.defaults['readOnly'] = false;
    CodeMirror.defaults['lineWrapping'] = true;
    
    if ( /Mac/.test( navigator.platform ))
        CodeMirror.defaults['extraKeys'] = {'Cmd-Space': 'autocomplete'};
    else
        CodeMirror.defaults['extraKeys'] = {'Ctrl-Space': 'autocomplete'};
    
    
    var editor = CodeMirror.fromTextArea( document.getElementById( 'inputExpression' ));
    $( '#inputExpression' )[0].editor = editor;
    $( editor.getInputField() ).keydown( function( event){
                                          return submitenter(this,event);
                                          });
    
    
    
    $(document).contextmenu({
                            delegate: ".Expression>.Out",
                            menu: [
                                   {title: "Copy TeX", cmd: "copyTeX"},
                                   {title: "Copy Yacas Expression", cmd: "copyYacasExpression"}
                                   ],
                            select: function(event, ui) {
                                parents = ui.target.parents('.Expression');
                                
                                if ( ui.cmd == "copyTeX"){
                                    yacas.copyToClipboard($(parents[0]).children('script')[0].textContent);
                                } else if ( ui.cmd == "copyYacasExpression"){
                                    yacas.copyToClipboard($(parents)[0].yacasExpression);
                                }
                            
                            },
                            preventContextMenuForPopup: true
                            });
                            
    
}


function changeMathJaxScale( newScale ){
    MathJax.Hub.Config({ "HTML-CSS": {
                            scale: newScale }
                       });
    MathJax.Hub.Queue(["Rerender", MathJax.Hub]);

}

function changeMathJaxFont( newFont ){
    MathJax.Hub.Config({ "HTML-CSS": {
                           preferredFont: newFont }
                       });
    MathJax.Hub.Queue(["Rerender", MathJax.Hub]);

}

var currentExpression = 1;
var numberOfLines = 1;


function updateInputNumber( updatedNumber) {
    $( "#inputCounter" ).html( "in " + updatedNumber + ":" );
}

function clearInput(){
    $( "#inputExpression" ).val("");
    $( '#inputExpression' )[0].editor.setValue("");
}


function submitenter( input, event ){
    if( event.which === 13 && event.shiftKey ){
        $( '#inputExpression' )[0].editor.save();
        value = $( '#inputExpression' )[0].editor.getValue();
        calculate( value );
        return false;
    }
    if( event.which === 38 && event.shiftKey ){
        if( !goUp( 0 ))
            return true;
    }
    
    return true;
}

function addInputEditor( lineid, number, value, rootElementID ){
    
    var $row = $( "<tr>", {class: 'In'} );
    
    var $tdnumber = $("<td>").append( "in  "+ number + ":");
    
    var $textarea = $( "<textarea>" , {class: 'InputTextarea'}).append( value );
    var $tdinput = $( "<td>" ).append( $textarea );
    
    $row.append( $tdnumber).append( $tdinput );

    $( rootElementID ).append( $row );
    
    var editor = CodeMirror.fromTextArea( $textarea[0] );
    
    editor.number = lineid;
    
    $textarea[0].editor = editor;
    editor.calculatedExpression = value;
    
    editor.on('keydown', function( editor, event ){
              
              if( event.which == 13 && event.shiftKey ){
                editor.save();
                event.preventDefault();
                processChange( editor.getValue(), editor.number, null  );
              }
              
              if( event.which == 38 && event.shiftKey ){
                goUp( editor.number );
    
              }
              if( event.which == 40 && event.shiftKey ){
                goDown( editor.number );
              }
              
    });
    
    editor.on('keyup', function (editor, e) {
          
        $tbody = $(editor.getTextArea()).parents("tbody");
        if ( $tbody.hasClass("New"))
            return;
          
        original = editor.calculatedExpression;
        currentValue = editor.getValue();
        
        if ( original != currentValue ){
              $tbody.addClass("Modified");
        }else{
              $tbody.removeClass("Modified");
        }
    });
    
    return editor;
}



function addOutput( lineid, number, rootElementID ){
    outputID = "output_" + lineid;

    var $row = $( "<tr>", {class: 'Out'} );

    $row.append( "<td>out "+ number + ":</td>"  );
    $row.append( "<td><div id='" + outputID+ "' ></div></td>" );
    
    $( rootElementID ).append( $row );
    $( "#" + outputID ).append( "<img src='yagy_ui/progressbar.indicator.gif' width='20' ></img>");
}

function addSideEffects( number, side_effects, rootElementID ){
    var row = $( "<tr></tr>" ).insertBefore( rootElementID );
    row.append("<td></td>" );
    row.append("<td><span>" + side_effects + "</span></td>");
}

function printResults( result ){
    number = result["idx"];
    outputID = "output_" + number;
    
    var ExpressionElement = $("#expression_"+number);
    
    if( result.hasOwnProperty( "side_effects" ) ){
        var outRow = ExpressionElement.children(".Out");
        addSideEffects(number, result["side_effects"].replace(/\n/g, '<br />'), outRow);
    }
    
    var output = $("#" + outputID);
    
    output.addClass( result["type"] );
    
    ExpressionElement.removeClass("Modified New Error Expression");
    ExpressionElement.addClass( result["type"]);

    output.text("");

    if( result["type"] === "Expression" ){

        output.addClass( "outside");
        output.append( "$$" + result["tex_code"] + "$$" );
        

 
        renderOutput( outputID );
        output[0].yacasExpression = result["expression"];
        output[0].lastWidth = $(output).width();
        
        if ( MathBar.supportsExpressionType( result["expression_type"], result["variables"].length )){
            
            var options = {};
            options["layout"] = "singleline";
            options["VIF"] = "max";
            options["type"] = result["expression_type"];
            
            if ( result["variables"].length > 0 ){
                options["type"] = options["type"] + "_" + result["variables"].length;
                options["defaultParameters"] = {};
                options["defaultParameters"]["variable"] = result["variables"];
            }
            
            var bar = new MathBar( outputID , options, function( result, outputID ){ parseMathBarResult( result, outputID )} );
            
        }
        
        $(output).resize(  debounce( function(event){
                         w = $(this)[0].lastWidth;
                         if ( $(this).width() != w ){
                            MathJax.Hub.Rerender(this);
                            $(this)[0].lastWidth = $(this).width();
                         }
                        }, 250));
    
    }else if( result["type"] === "Error" ){

        output.append( result["error_message"] );

    }else if( result["type"] === "Plot2D" ){

        $.plot(output, result["plot2d_data"] );

        var width = $("#" + outputID).width();
        
        output.resizable({ maxWidth: width, minWidth: 200, minHeight: 200} );
        output.addClass( "resizable" );
        
    }else if( result["type"] === "Plot3D" ){

        var width = $("#" + outputID).width();
        var height = 300;

        webGLEnabled = yacas.isWebGLEnabled();
        
        var plot3d = new Plot3D(result["plot3d_data"], width, height);
        
        plot3d.setRenderer( webGLEnabled );
        plot3d.addLegend("#" + outputID);
        
        output.append(plot3d.renderer.domElement);
        output[0].plot3D = plot3d;

        var controls = new THREE.TrackballControls(plot3d.camera, plot3d.renderer.domElement);
        controls.addEventListener( 'change', ControlsChanged );
        controls.enabled = false;
        output[0].controls = controls;
        plot3d.renderer.render(plot3d.scene, plot3d.camera);

        function render() {
            requestAnimationFrame(render);
            controls.update();
        }
        
        render();
        
        output.resizable({ maxWidth: width, minWidth: 200, minHeight: 200} );
        output.addClass( "resizable" );
        output.resize( function(){ Plot3dResized( this );});
        output.mouseout( function(event){ Plot3dMouseOut( this, event );});
        output.click( function(event){ Plot3dClicked( this, event );});
        
    } else if (result["type"] === "Graph") {
        var vertices = result["graph_vertices"];
        var no_vertices = vertices.length;
        
        var vis_vertices = [];

        for (var i = 0; i < no_vertices; ++i)
            vis_vertices.push({id: i + 1, label: vertices[i]});

        var edges = result["graph_edges"];
        var no_edges = edges.length;

        var vis_edges = [];

        for (var i = 0; i < no_edges; ++i) {
            var arrows = 'to';
            if (edges[i].bi)
                arrows += ',from';
            vis_edges.push({from: edges[i].from, to: edges[i].to, arrows: arrows});
        }



        var data = {
          nodes: vis_vertices,
          edges: vis_edges
        };
        var options = {};
        
        var network = new vis.Network(output[0], data, options);

        var width = output.width();
        
        output.resizable({ maxWidth: width, minWidth: 200, minHeight: 200} );
        output.addClass( "resizable" );
    
    
    }
}

function ControlsChanged( element, event ){
    plot3d = element.target.domElement.parentElement.plot3D;
    plot3d.renderer.render(plot3d.scene, plot3d.camera);
}

function Plot3dResized( output ){
    var height = $(output).height();
    var width = $(output).width();
    plot3d = output.plot3D;
    plot3d.resizePlot( width, height);
    plot3d.renderer.render(plot3d.scene, plot3d.camera);
}

function Plot3dMouseOut( output, event ){
    controls = output.controls;
    controls.enabled = false;
    $(output).removeClass( "Plot3DActive");
}

function Plot3dClicked( output, event ){
    controls = output.controls;
    controls.enabled = true;
    $(output).addClass( "Plot3DActive");
}

function renderOutput( outputID ){
    MathJax.Hub.Queue( ["Typeset", MathJax.Hub, outputID] );
}

function removeOldResults( number ){
    $( "#expression_" + number ).remove();
}

function addExpressionCells( lineID, expressionid, value, rootElementID, expression_class){
    
    
    $("<tbody>", {id: 'expression_' + lineID , class: expression_class}).insertBefore( rootElementID );
    //addEditable( lineID, expressionid, value, "#expression_" + lineID);
    addInputEditor( lineID, expressionid, value, "#expression_" + lineID);
    addOutput( lineID, expressionid, "#expression_" + lineID);
}

function calculateAt( value, rootElementId ) {
    addExpressionCells( numberOfLines, currentExpression, value, '#' + rootElementId, "New");
    
    yacas.eval( numberOfLines, value );
    
    currentExpression++;
    numberOfLines++;
    
    
}

function calculate( value ){
    calculateAt(value, 'expression_0');
    clearInput();
}

function parseMathBarResult( value, outputID ){
    number = outputID.split("_")[1];
    nextNumber = findNextExpression(number);
    calculateAt( value, "expression_" + nextNumber);
    goto( nextNumber );
}

function processChange( value, number, object ){
   var decodedValue = $("<div/>").html( value ).text();
    
    if ( $("#expression_"+number).hasClass("NotToCalculate")){
        $("#expression_"+number).removeClass("NotToCalculate");
        return;
    }
    
    expression_class = $("#expression_"+number).attr('class');
    
    addExpressionCells( numberOfLines, currentExpression, decodedValue, "#expression_" + number, expression_class);
    
    yacas.eval( numberOfLines, decodedValue );
    
    goDown( number );
    
    currentExpression++;
    numberOfLines++;
    
    
    removeOldResults( number );

}



function evaluateCurrent(){
    var $tbody = $(document.activeElement).parents("tbody");
    var $input = $tbody.find(".InputTextarea");

    if ( $input.length ){
        editor = $input[0].editor;
        processChange( editor.getValue(), editor.number, null  );
        return;
    }

    $input = $tbody.find("#inputExpression");


    if ( $input.length ){
        inputVal = $input[0].editor.getValue();
        if ( inputVal !== "" ) calculate( inputVal );
    }
}



function evaluateAll(){
    $(".InputTextarea").each( function() {
                             editor = this.editor;
                             //editor.save();
                             processChange( editor.getValue(), editor.number, null  );

                        
                        });
    inputVal = $( "#inputExpression" )[0].editor.getValue();
    if ( inputVal !== "" ) calculate( inputVal );
    $("#inputExpression").focus();
}




function getAllInputs(){
    var inputs = [];
    $(".InputTextarea").each( function() {
                       inputs.push( this.editor.getValue());
                
                       });
    inputVal = $( "#inputExpression" )[0].editor.getValue();
    if ( inputVal !== "" ) inputs.push( inputVal );
    return inputs;
}


function findPreviousExpression( number ){
    var previous = $("#expression_"+ number).prev("tbody");
    if ( previous.length === 0 ) return null; //First row
    return previous[0].id.split("_")[1];
    
}

function findNextExpression( number ){
    var next = $("#expression_"+ number).next("tbody");
    if ( next.length === 0 ) return null; //First row
    return next[0].id.split("_")[1];
    
}

function previousCell() {
    var focused = $(':focus').parents("tbody");
    
    if (focused.length === 0 ){
        return;
    }
    
    var number = $(focused)[0].id.split("_")[1];
    goUp( number );
}

function nextCell() {
    var focused = $(':focus').parents("tbody");
    
    if (focused.length === 0 ){
        return;
    }
    
    var number = $(focused)[0].id.split("_")[1];
    goDown( number );
}

function goUp( number ){
    prev = findPreviousExpression( number );
    if ( prev === null ) return false;
    goto ( prev );
    return true;    
}

function goDown( number ){
    next = findNextExpression( number );
    if ( next === null ) return false;
    goto ( next );
    return true;
}

function goto( number ){
    if ( number === '0' ){
        //$("#inputExpression").focus();
        $( '#inputExpression' )[0].editor.focus();
    }else {
        //$("#expression_"+number).find(".editable").click();
        $("#expression_"+number).find("textarea")[0].editor.focus();
    }
    
}

function insertElement( whetherAfterOrBefore ){
    var focused = $(':focus').parents("tbody");
    
    if (focused.length === 0 ){
        return;
    }
    
    var element = $("<tbody id='expression_" + numberOfLines + "' class='New'></tbody");
    var value = "";
    var clickNew = true;
    
    //Special case when inserting after last input (expression_0)
    if ( whetherAfterOrBefore === "after" && $(focused)[0].id === "expression_0"){
        whetherAfterOrBefore = "before";
        value = $("#inputExpression").val();
        clearInput();
        clickNew = false;
    }
    
    if( whetherAfterOrBefore === "before"){
        element.insertBefore( focused );
    }else{
        element.insertAfter( focused );
    }
    
    //var editable = addEditable(numberOfLines, "", value, "#expression_" + numberOfLines);
    var editor = addInputEditor( numberOfLines, "", value, "#expression_" + numberOfLines);
    
    if ( clickNew ){
        //editable.click();
        editor.focus();
    }
    numberOfLines++;
}

function insertAfterCurrent(){
    insertElement( "after" );
}

function insertBeforeCurrent(){
    insertElement( "before" );
}

function deleteCurrent(){
    var focused = $(':focus').parents("tbody");
    
    if (focused.length === 0 ){
        return;
    }
    
    //Cannot delete last input (expression_0)
    if ( $(focused)[0].id === "expression_0"){
        return;
    }
    number = $(focused)[0].id.split("_")[1];
    goDown( number );
    $(focused).remove();

}

function contextHelp() {
    var e = document.activeElement;
    yacas.help(e.value, e.selectionStart);
}

function debounce(func, wait, immediate) {
    var timeout;
    return function() {
        var context = this, args = arguments;
        var later = function() {
            timeout = null;
            if (!immediate) func.apply(context, args);
        };
        var callNow = immediate && !timeout;
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow) func.apply(context, args);
    };
};

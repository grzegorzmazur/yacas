/*
 * Autosize textarea for Jeditable dedicated for YAGY
 *
 *
 */
 
$.editable.addInputType( 'autogrow', {
    element : function( settings, original ) {
        var textarea = $('<textarea />');
        if ( settings.rows ) {
            textarea.attr('rows', settings.rows);
        } else {
            textarea.height(settings.height);
        }
        if ( settings.cols ) {
            textarea.attr('cols', settings.cols);
        } else {
            textarea.width(settings.width);
        }
        $( this ).append( textarea );
        return( textarea );
    },
    plugin : function( settings, original ) {
                        
        this[0].editor = CodeMirror.fromTextArea($( 'textarea', this )[0], {lineNumbers: false, mode: {name: "javascript", globalVars: true},matchBrackets: true });

                        
        $( 'textarea', this ).autosize();
        $( 'textarea', this ).keydown( function (e) {
                                var number = this.name;
                                if( e.which == 13 && e.shiftKey ){
                                      $(this).closest( "form" )[0].editor.save();
                                      $(this).closest( "form" ).submit();
                                }
                                //if( e.which == 13 ) e.preventDefault();
                                if( e.which == 38 && e.shiftKey ){
                                    if( !goUp( number ))
                                      return;
                                }
                                if( e.which == 40 && e.shiftKey ){
                                    if( !goDown( number ))
                                        return;
                                }
                                      
                            });
        $( 'textarea', this ).keyup( function (e) {
                                
                                if ( $(this).parents("tbody").hasClass("New"))
                                    return;
                                    
                                original = $(this).parents("span")[0].calculatedExpression;
                                if ( original != this.value ){
                                      $(this).parents("tbody").addClass("Modified");
                                }else{
                                      $(this).parents("tbody").removeClass("Modified");
                                }
                            });
                                      

                                      
                                
    }
});

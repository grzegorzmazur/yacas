/* global CodeMirror, MathBar, MathJax, Plotly, vis, yacas */

"use strict";

//Function stops scrolling event, and just scrool the window
//Otherwise Code-Mirror editor kept scrolling one line up even if there was no line to scroll
function scrollListener(e) {

    const dx = e.wheelDeltaX;
    const dy = -1 * e.wheelDeltaY;

    window.scrollBy(dx, dy);
    e.preventDefault();
}

function findPreviousExpression(number) {
    var previous = $("#expression_" + number).prev("tbody");
    if (previous.length === 0)
        return null; //First row
    return previous[0].id.split("_")[1];

}

function findNextExpression(number) {
    var next = $("#expression_" + number).next("tbody");
    if (next.length === 0)
        return null; //First row
    return next[0].id.split("_")[1];

}

function goto(number) {
    if (number === "0")
        $("#inputExpression")[0].editor.focus();
    else
        $("#expression_" + number).find("textarea")[0].editor.focus();
}

function goUp(number) {
    var prev = findPreviousExpression(number);
    if (prev === null)
        return false;
    goto(prev);
    return true;
}

function goDown(number) {
    var next = findNextExpression(number);
    if (next === null)
        return false;
    goto(next);
    return true;
}

function submitenter(input, event) {
    if (event.which === 13 && event.shiftKey) {
        $("#inputExpression")[0].editor.save();
        const value = $("#inputExpression")[0].editor.getValue();
        calculate(value);
        return false;
    }
    if (event.which === 38 && event.ctrlKey) {
        goUp(0);
        return false;
    }

    return true;
}

function hint(cm, option) {
    return new Promise(function (accept) {
        var cursor = cm.getCursor();
        var line = cm.getLine(cursor.line);

        var start = cursor.ch;
        var end = cursor.ch;

        while (start && /\w/.test(line.charAt(start - 1)))
            --start;

        while (end < line.length && /\w/.test(line.charAt(end)))
            ++end;

        var word = line.slice(start, end);

        yacas.complete(word, function (hints) {
            return accept({
                list: hints,
                from: CodeMirror.Pos(cursor.line, start),
                to: CodeMirror.Pos(cursor.line, end)
            });
        });
    });
}

function load() {

    MathBar.initializeFunctions("mathbar/functions.json");

    //To keep CodeMirror editor from bad scrolling
    document.body.addEventListener("mousewheel", function (e) {
        scrollListener(e);
    });

    $("#inputExpression").autosize();

    $(window).on("resize", function (event) {

        var w = $("body").width() - $("#Elements>tbody>tr>td:first-child").innerWidth() - 10; //10 is for padding in the td.Out element
        $(".resizable").each(function () {
            var maxwidth = $(this).resizable("option", "maxWidth");
            if ($(this).width() > w || $(this).width() === maxwidth)
                $(this).width(w);
        });
        $(".resizable").resizable("option", "maxWidth", w);
        // $( ".CodeMirror").width( w - 2 );

        return true;
    });

    CodeMirror.defaults["lineNumbers"] = false;
    CodeMirror.defaults["mode"] = { name: "yacas" };
    CodeMirror.defaults["matchBrackets"] = true;
    CodeMirror.defaults["autoCloseBrackets"] = '()[]{}""';
    CodeMirror.defaults["readOnly"] = false;
    CodeMirror.defaults["lineWrapping"] = true;
    CodeMirror.defaults["hintOptions"] = { hint: hint };

    if (/Mac/.test(navigator.platform))
        CodeMirror.defaults["extraKeys"] = { "Cmd-Space": "autocomplete" };
    else
        CodeMirror.defaults["extraKeys"] = { "Ctrl-Space": "autocomplete" };


    var editor = CodeMirror.fromTextArea(document.getElementById("inputExpression"));
    $("#inputExpression")[0].editor = editor;
    $(editor.getInputField()).keydown(function (event) {
        return submitenter(this, event);
    });
    editor.on("change", function () {
        if (yacas.hasOwnProperty("on_contentsChanged"))
            yacas.on_contentsChanged();
    });
    $(document).contextmenu({
        delegate: ".Expression>.Out",
        menu: [
            { title: "Copy TeX", cmd: "copyTeX" },
            { title: "Copy Yacas Expression", cmd: "copyYacasExpression" }
        ],
        select: function (event, ui) {
            const parents = ui.target.parents(".Expression");

            if (ui.cmd === "copyTeX")
                yacas.copyToClipboard($(parents[0]).children("script")[0].textContent);
            else if (ui.cmd === "copyYacasExpression")
                yacas.copyToClipboard($(parents)[0].yacasExpression);
        },
        preventContextMenuForPopup: true
    });
}


function changeMathJaxScale(newScale) {
    MathJax.Hub.Config({
        CommonHTML: {
            scale: newScale
        }
    });
    MathJax.Hub.Queue(["Rerender", MathJax.Hub]);

}

function changeMathJaxFont(newFont) {
    MathJax.Hub.Config({
        CommonHTML: {
            preferredFont: newFont
        }
    });
    MathJax.Hub.Queue(["Rerender", MathJax.Hub]);

}

var currentExpression = 1;
var numberOfLines = 1;


function updateInputNumber(updatedNumber) {
    $("#inputCounter").html("in " + updatedNumber + ":");
}

function clearInput() {
    $("#inputExpression").val("");
    $("#inputExpression")[0].editor.setValue("");
}

function addInputEditor(lineid, number, value, rootElementID) {

    var $row = $("<tr>", { class: "In" });

    var $tdnumber = $("<td>").append("in  " + number + ":");

    var $textarea = $("<textarea>", { class: "InputTextarea" }).append(value);
    var $tdinput = $("<td>").append($textarea);

    $row.append($tdnumber).append($tdinput);

    $(rootElementID).append($row);

    var editor = CodeMirror.fromTextArea($textarea[0]);

    editor.number = lineid;

    $textarea[0].editor = editor;
    editor.calculatedExpression = value;

    editor.on("keydown", function (editor, event) {

        if (event.which === 13 && event.shiftKey) {
            editor.save();
            event.preventDefault();
            processChange(editor.getValue(), editor.number, null);
        }

        if (event.which === 38 && event.ctrlKey)
            goUp(editor.number);

        if (event.which === 40 && event.ctrlKey)
            goDown(editor.number);

    });

    editor.on("keyup", function (editor, e) {

        var $tbody = $(editor.getTextArea()).parents("tbody");
        if ($tbody.hasClass("New"))
            return;

        var original = editor.calculatedExpression;
        var currentValue = editor.getValue();

        if (original !== currentValue) {
            $tbody.addClass("Modified");
            yacas.on_contentsChanged();
        } else {
            $tbody.removeClass("Modified");
        }
    });

    return editor;
}

function addOutput(lineid, number, rootElementID) {
    var outputID = "output_" + lineid;

    var $row = $("<tr>", { class: "Out" });

    $row.append("<td>out " + number + ":</td>");
    $row.append("<td><div id='" + outputID + "' ></div></td>");

    $(rootElementID).append($row);
    $("#" + outputID).append("<img src='yacas_gui/progressbar.indicator.gif' width='20' ></img>");
}

function addSideEffects(number, side_effects, rootElementID) {
    var row = $("<tr></tr>").insertBefore(rootElementID);
    row.append("<td></td>");
    row.append("<td><span>" + side_effects + "</span></td>");
}

function printResults(result) {
    var number = result["idx"];
    var outputID = "output_" + number;

    var ExpressionElement = $("#expression_" + number);

    if (result.hasOwnProperty("side_effects")) {
        var outRow = ExpressionElement.children(".Out");
        addSideEffects(number, result["side_effects"].replace(/\n/g, "<br />"), outRow);
    }

    var output = $("#" + outputID);

    output.addClass(result["type"]);

    ExpressionElement.removeClass("Modified New Error Expression");
    ExpressionElement.addClass(result["type"]);

    output.text("");

    if (result["type"] === "Expression") {

        output.addClass("outside");
        output.append("$$" + result["tex_code"] + "$$");



        renderOutput(outputID);
        output[0].yacasExpression = result["expression"];
        output[0].lastWidth = $(output).width();

        if (MathBar.supportsExpressionType(result["expression_type"], result["variables"].length)) {

            let options = {};
            options["layout"] = "singleline";
            options["VIF"] = "max";
            options["type"] = result["expression_type"];

            if (result["variables"].length > 0) {
                options["type"] = options["type"] + "_" + result["variables"].length;
                options["defaultParameters"] = {};
                options["defaultParameters"]["variable"] = result["variables"];
            }

            var bar = new MathBar(outputID, options, function (result, outputID) {
                parseMathBarResult(result, outputID);
            });

        }

        $(output).resize(debounce(function (event) {
            var w = $(this)[0].lastWidth;
            if ($(this).width() !== w) {
                MathJax.Hub.Rerender(this);
                $(this)[0].lastWidth = $(this).width();
            }
        }, 250));

    } else if (result["type"] === "Error") {

        output.append(result["error_message"]);

    } else if (result["type"] === "Plot2D") {
        let data = [];

        for (let i = 0; i < result["plot2d_data"].length; ++i) {
            let x = [];
            let y = [];
            const d = result["plot2d_data"][i]["data"];
            for (let j = 0; j < d.length; ++j) {
                x.push(d[j][0]);
                y.push(d[j][1]);
            }
            data.push({
                type: "scatter",
                name: result["plot2d_data"][i]["label"],
                x: x,
                y: y,
            });
        }

        const layout = {
            autosize: true,
            margin: {
                l: 0,
                r: 0,
                b: 0,
                t: 16
            },
            xaxis: {
                automargin: true,
            },
            yaxis: {
                automargin: true,
            }
        };

        const options = {
            responsive: true
        };

        Plotly.newPlot(output[0], data, layout, options);

        output.resizable({
            handles: "s,e",
            minWidth: 200,
            minHeight: 200,
            resize: function (e, info) {
                window.console.log("resize", info);
                Plotly.relayout(output[0], { height: info["size"]["height"] });
                return true;
            }
        });

        output.addClass("resizable");

    } else if (result["type"] === "Plot3D") {
        let data = [];

        for (let i = 0; i < result["plot3d_data"].length; ++i) {
            let x = [];
            let y = [];
            let z = [];
            const d = result["plot3d_data"][i]["data"];
            for (let j = 0; j < d.length; ++j) {
                x.push(d[j][0]);
                y.push(d[j][1]);
                z.push(d[j][2]);
            }
            data.push({
                type: "mesh3d",
                opacity: 0.8,
                x: x,
                y: y,
                z: z
            });
        }

        const layout = {
            autosize: true,
            margin: {
                l: 0,
                r: 0,
                b: 8,
                t: 16
            }
        };

        const options = {
            responsive: true
        };

        Plotly.newPlot(output[0], data, layout, options);

        output.resizable({
            handles: "s",
            minWidth: 200,
            minHeight: 200,
            resize: function (e, info) {
                Plotly.relayout(output[0], { height: info["size"]["height"] });
                return true;
            }
        });

        output.addClass("resizable");

    } else if (result["type"] === "Graph") {
        var vertices = result["graph_vertices"];
        var no_vertices = vertices.length;

        var vis_vertices = [];

        for (var i = 0; i < no_vertices; ++i)
            vis_vertices.push({ id: i + 1, label: vertices[i] });

        var edges = result["graph_edges"];
        var no_edges = edges.length;

        var vis_edges = [];

        for (var i = 0; i < no_edges; ++i) {
            var arrows = "to";
            if (edges[i].bi)
                arrows += ",from";
            vis_edges.push({ from: edges[i].from, to: edges[i].to, arrows: arrows });
        }



        var data = {
            nodes: vis_vertices,
            edges: vis_edges
        };
        var options = {};

        var network = new vis.Network(output[0], data, options);

        var width = output.width();

        output.resizable({ maxWidth: width, minWidth: 200, minHeight: 200 });
        output.addClass("resizable");
    }
}

function ControlsChanged(element, event) {
    var plot3d = element.target.domElement.parentElement.plot3D;
    plot3d.renderer.render(plot3d.scene, plot3d.camera);
}

function renderOutput(outputID) {
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, outputID]);
}

function removeOldResults(number) {
    $("#expression_" + number).remove();
}

function addExpressionCells(lineId, expressionId, value, rootElementId, expressionClass) {
    $("<tbody>", { id: "expression_" + lineId, class: expressionClass }).insertBefore(rootElementId);
    addInputEditor(lineId, expressionId, value, "#expression_" + lineId);
    addOutput(lineId, expressionId, "#expression_" + lineId);
}

function calculateAt(value, rootElementId) {
    addExpressionCells(numberOfLines, currentExpression, value, "#" + rootElementId, "New");

    yacas.eval(numberOfLines, value);

    currentExpression++;
    numberOfLines++;
}

function calculate(value) {
    calculateAt(value, "expression_0");
    clearInput();
}

function parseMathBarResult(value, outputID) {
    const number = outputID.split("_")[1];
    const nextNumber = findNextExpression(number);
    calculateAt(value, "expression_" + nextNumber);
    goto(nextNumber);
}

function processChange(value, number, object) {
    var decodedValue = $("<div/>").html(value).text();

    if ($("#expression_" + number).hasClass("NotToCalculate")) {
        $("#expression_" + number).removeClass("NotToCalculate");
        return;
    }

    var expression_class = $("#expression_" + number).attr("class");

    addExpressionCells(numberOfLines, currentExpression, decodedValue, "#expression_" + number, expression_class);

    yacas.eval(numberOfLines, decodedValue);

    goDown(number);

    currentExpression++;
    numberOfLines++;

    removeOldResults(number);
}

function evaluateCurrent() {
    var $tbody = $(document.activeElement).parents("tbody");
    var $input = $tbody.find(".InputTextarea");

    if ($input.length) {
        var editor = $input[0].editor;
        processChange(editor.getValue(), editor.number, null);
        return;
    }

    $input = $tbody.find("#inputExpression");


    if ($input.length) {
        var inputVal = $input[0].editor.getValue();
        if (inputVal !== "")
            calculate(inputVal);
    }
}


function evaluateAll() {
    $(".InputTextarea").each(function () {
        var editor = this.editor;
        processChange(editor.getValue(), editor.number, null);
    });

    var $input = $("#inputExpression");
    if ($input.length) {
        var inputVal = $input[0].editor.getValue();
        if (inputVal !== "")
            calculate(inputVal);
    }

    $input.focus();
}



function getAllInputs() {
    let inputs = [];
    $(".InputTextarea").each(function () {
        inputs.push(this.editor.getValue());

    });
    let inputVal = $("#inputExpression")[0].editor.getValue();
    if (inputVal !== "")
        inputs.push(inputVal);
    return inputs;
}

function previousCell() {
    var focused = $(":focus").parents("tbody");

    if (focused.length === 0) {
        return;
    }

    var number = $(focused)[0].id.split("_")[1];
    goUp(number);
}

function nextCell() {
    var focused = $(":focus").parents("tbody");

    if (focused.length === 0) {
        return;
    }

    var number = $(focused)[0].id.split("_")[1];
    goDown(number);
}


function insertElement(whetherAfterOrBefore) {
    var focused = $(":focus").parents("tbody");

    if (focused.length === 0) {
        return;
    }

    var element = $("<tbody id='expression_" + numberOfLines + "' class='New'></tbody");
    var value = "";
    var clickNew = true;

    //Special case when inserting after last input (expression_0)
    if (whetherAfterOrBefore === "after" && $(focused)[0].id === "expression_0") {
        whetherAfterOrBefore = "before";
        value = $("#inputExpression").val();
        clearInput();
        clickNew = false;
    }

    if (whetherAfterOrBefore === "before") {
        element.insertBefore(focused);
    } else {
        element.insertAfter(focused);
    }

    //var editable = addEditable(numberOfLines, "", value, "#expression_" + numberOfLines);
    var editor = addInputEditor(numberOfLines, "", value, "#expression_" + numberOfLines);

    if (clickNew) {
        //editable.click();
        editor.focus();
    }
    numberOfLines++;
}

function insertAfterCurrent() {
    insertElement("after");
}

function insertBeforeCurrent() {
    insertElement("before");
}

function deleteCurrent() {
    var focused = $(":focus").parents("tbody");

    if (focused.length === 0) {
        return;
    }

    //Cannot delete last input (expression_0)
    if ($(focused)[0].id === "expression_0") {
        return;
    }
    const number = $(focused)[0].id.split("_")[1];
    goDown(number);
    $(focused).remove();

}

function contextHelp() {
    var e = document.activeElement;
    yacas.help(e.value, e.selectionStart);
}

function debounce(func, wait, immediate) {
    var timeout;
    return function () {
        var context = this, args = arguments;
        var later = function () {
            timeout = null;
            if (!immediate)
                func.apply(context, args);
        };
        var callNow = immediate && !timeout;
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow)
            func.apply(context, args);
    };
}

function exportScript() {
    var elems = document.getElementsByClassName("InputTextarea");
    var r = [];
    for (var i = 0; i < elems.length; ++i) {
        var s = elems[i].value;
        if (!s.endsWith(";"))
            s += ";";
        r.push(s)
    }
    return r;
}

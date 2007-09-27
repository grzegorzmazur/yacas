
// This file has some functions needed in pages where commands need to be sent to a Java applet.


var prevOnLoadTutorial = window.onload;
window.onload = initPage;

function initPage()
{
  var viewportSize = getViewportSize();

  var preferredWidth = viewportSize[0]-400;
  var preferredHeight = viewportSize[1]-100;
  if (preferredWidth  < 320) preferredWidth  = 320;
  if (preferredHeight < 200) preferredHeight = 200;

  if (prevOnLoadTutorial)
    prevOnLoadTutorial();

  if (!document.getElementsByTagName)
  { 
    return; 
  }
  {
    var popups = document.getElementById("popups");
    if (popups)
    {
      popups.style.height = preferredHeight+"px";
    }
  }
  
  {
    var links = document.getElementsByTagName("a");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "commandlink")
        {
          object.id = "command"+i;
          object.onmouseover=commandLinkOver;
          object.onmouseout=commandLinkOut;
          object.onclick=commandLinkClick;
        }
        if (object.className == "linkcalc")
        {
          object.onmouseover=calcLinkOver;
          object.onmouseout=calcLinkOut;
        }
      }
    }
  }

  {
    var links = document.getElementsByTagName("div");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "math")
        {
          var width = "600px";//object.style.width;
          var height = "80px";//object.style.height;
          var expression = object.innerHTML;

          if (object.style)
            if (object.style.height)
            {
              height = object.style.height;
            }

          object.innerHTML = 
            '<applet code=net.sf.yacas.FormulaViewApplet archive="yacas.jar" width="'+width+'" height="'+height+'" >'+
            '  <param name="expression" value="'+expression+'" />'+ expression +
            '</applet><br />';

        }
      }
    }
  }

  {
    var links = document.getElementsByTagName("span");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "javawarn")
        {
          object.innerHTML = 
    '<p style="color:#FF0000;">' + 
    '  Your browser does not have an up to date Java system installed. This web site uses Java ' + 
    '  to allow you to do calculations immediately online. Please visit <a href="http://www.java.com/">www.java.com</a>' + 
    '  to download and install the latest version for free.' + 
    '</p>' +
    '<p style="color:#FF0000;">' + 
    '  Alternatively you can go to the <a href="downloads.html">downloads</a> part of our web site and download <i>yacas</i> for use off-line.' + 
    '</p>';

        }
        else if (object.className == "codeEdit")
        {
          var width = preferredWidth;
          var height = preferredHeight;

          object.innerHTML = 
'      <table cellspacing=0>' +
'        <tr>' +
'          <td style="border-color:black; border-style:solid; border-width:thin; border-bottom:0px; background-color: #DDDDEE;">' +
'          <span>Code editor<\/span>' +
'          <\/td>' +
'        <\/tr>' +
'        <tr>' +
'          <td style="border-color:black; border-style:solid; border-width:thin; background-color:#fafabe;">' +
'            <textarea style="background-color:#fafabe; width:'+width+'px; height:'+height+'px; " class="codeEditor" id="codeText"><\/textarea>' +
'          <\/td>' +
'        <\/tr>' +
'      <\/table>';
          if (object.id)
            initCodeEditors(object.id);
        }
        else if (object.className == "yacasConsole")
        {
          var width  = preferredWidth;
          var height = preferredHeight;

          var jarFile = "yacas.jar";
          var programMode = "console";
          if (object.id)
          {
            programMode = object.id;
            if (object.id == "journal")
            {
              width += 300;
            }
            if (object.id == "tutorial")
            {
              width  = 750;
              height = 200;
            }
          }
          object.innerHTML = 

'    <table cellspacing=0>' + 
'      <tr>' + 
'        <td style="border-color:black; border-style:solid; border-width:thin; border-bottom:0px; background-color: #DDDDEE;">' + 
'        <span>Yacas calculation center<\/span>' + 
'        <\/td>' + 
'      <\/tr>' + 
'      <tr>' + 
'        <td style="border-color:black; border-style:solid; border-width:thin;">' + 
'          <APPLET id="consoleApplet" name="consoleApplet" archive="yacas.jar" code="net.sf.yacas.ConsoleApplet" width='+width+' height='+height+' alt="Java support seems to be disabled in your browser, so the console is not available. If you want to be able to use Yacas online, please enable Java support. Alternatively, you can go to the About Yacas tab to download Yacas and run it locally.">' + 
'            <PARAM name="programMode" value="'+programMode+'" \/>' + 
'            <PARAM name="progressbar" value="true" \/>' + 
'            <PARAM name="boxmessage" value="Loading Yacas..." \/>' + 
'            <PARAM NAME="init1" value="Load(\'\'yacasinit.ys\'\')" \/>' + 
'            <PARAM NAME="init2" value="Macro(TeXWrite,{x})WriteString(TeXForm(Hold(@x)))">' + 
'            <PARAM NAME="init3" value="PrettyPrinter\'Set(\'\'TeXWrite\'\')">' + 
'Java support does not seem to be installed in your browser, so the console is not available. ' + 
'If you want to be able to use Yacas online, please install and enable Java support, which can be downloaded from http:\/\/www.java.com\/ for free. ' + 
'Alternatively, you can go to the "About Yacas" tab to download Yacas and run it locally.' + 
'          <\/APPLET>' + 
'        <\/td>' + 
'      <\/tr>' + 
'    <\/table>';

        }
        else if (object.className == "yacasExpression")
        {
          object.innerHTML = "<tt><b>"+object.innerHTML+"</b><tt>";
        }
        else if (object.className == "yacasCodeSnippet")
        {
          object.innerHTML = 
'<table width="100%">' +
'<tr><td width=100% bgcolor="#DDDDEE"><pre>' +
object.innerHTML +
'</pre></tr>' +
'</table>';
        }




      }
    }
  }
}

var initCodeEditors_count = 0;
function initCodeEditors(area)
{
  var datahub = getDatahub();
  {
    var links = document.getElementsByTagName("textarea");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "codeEditor")
        {
          if (datahub)
          {
            try
            {
              datahub.setProgramMode(area);
              object.value = datahub.getArticle();
            }
            catch (e) 
            {
              initCodeEditors_count = initCodeEditors_count + 1;
              if (initCodeEditors_count < 10)
              {
                setTimeout("initCodeEditors('"+area+"')",1000);
              }
            }
            object.focus();
          }
        }
      }
    }
  }
}


function commandLinkOver()
{
  this.style.cursor='pointer';
  this.style.color="#AAE";
}

function commandLinkOut()
{
  this.style.color="#0000FF";
}

function calcLinkOver()
{
  this.style.cursor='pointer';
  this.style.color="#AAE";
}
function calcLinkOut()
{
  this.style.cursor='pointer';
  this.style.color="#0000ff";
}


function getPlainText(text)
{
  if (text.search('&') >= 0) 
  {
    text = text.replace(/&lt;/g,'<');
    text = text.replace(/&gt;/g,'>');
    text = text.replace(/&quot;/g,'"');
    text = text.replace(/&amp;/g,'&');
  }
  return text;
}

function commandLinkClick()
{
  var id = this.id;
  var expression = getPlainText(this.innerHTML);
  startExpressionAnimation(id,expression);
}

function getConsoleApplet()
{
  var consoleApplet = document.consoleApplet;
  if (!consoleApplet)
    consoleApplet = parent.document.consoleApplet;
  if (consoleApplet)
  {
    if (consoleApplet.isActive)
      if (!consoleApplet.isActive())
      {
        alert("Trying to execute an expression while the calculation center has not been initialized yet");
        consoleApplet = null;
      }
  }
  return consoleApplet;
}

function yacasEval(expression)
{ 
  var consoleApplet = getConsoleApplet();
  if (consoleApplet)
  {
    expression = getPlainText(expression);
    consoleApplet.AddInputLine(expression);
    consoleApplet.InvokeCalculationSilent(expression);
  }
}

function yacasStopCurrentCalculation()
{
  var consoleApplet = getConsoleApplet();
  if (consoleApplet)
  {
    consoleApplet.StopCurrentCalculation();
  }  
}

// Do a calculation, returning the result as a string
function yacas_calculate(expression)
{ 
  var consoleApplet = getConsoleApplet();
  if (consoleApplet)
  {
    return consoleApplet.calculate(getPlainText(expression));
  }
  return "False";
}

function yacas_getlasterror()
{ 
  var consoleApplet = document.consoleApplet;
  if (!consoleApplet)
    consoleApplet = parent.document.consoleApplet;
  if (consoleApplet)
  {
    return consoleApplet.getLastError();
  }
  return "";
}

function commandEdit(base)
{
  var elem = parent.document.getElementById('ConsoleFrame');
  if (elem)
  {
    elem.contentWindow.document.location.href = base+"edit.html"+document.location.search;
  }
}

function checkDatahubAvailable(datahub,loc)
{
  if (datahub)
  {
    if (datahub.isActive)
    {
      if (!datahub.isActive())
      {
        alert("Data hub not yet initialized: "+loc);
      }
    }
  }
}

function keepArticle()
{
  var datahub = getDatahub();
  checkDatahubAvailable(datahub,"keepArticle");
  if (datahub)
  {
    var elem = document.getElementById('codeText');
    if (elem)
    {
      if (datahub)
      {
        try 
        {
          datahub.setArticle(elem.value);
        } 
        catch (e) 
        {
          alertUpgradeJava();
        }
      }
    }
  }
}
function commandView(frame,base)
{
  keepArticle();
  {
    var elem = parent.document.getElementById(frame);
    if (elem)
    {
      elem.contentWindow.document.location.href = "consoleview.html";
    }
  }
}

function gotoHREF(href) 
{
  document.location.href = href;
}

function getBase()
{
  var seach;
  seach = document.location.search;
  if (seach.indexOf("?") == 0)
  {
    seach = seach.substring(1);
  }
  return seach;
}


function generateExample()
{
  var datahub = getDatahub();
  checkDatahubAvailable(datahub,"generateExample");

//alert("1+1 = "+yacas_calculate("1+1;"));

  if (datahub)
  {
    if (datahub.getNrExamples() == "0")
    {
      alert("No examples defined yet. You can define examples in the article by adding entries of the form {{example: ...example... :example}}");
    }
    else
    {
      var text = datahub.getExample();
//alert("got from datahub: "+text);
      yacasEval(""+text);
    }
  }
}


function getViewportSize()
{
  var size = [0,0];

  if (typeof window.innerWidth != 'undefined')
  {
    size = [ window.innerWidth, window.innerHeight ];
  }
  else if (typeof document.documentElement != 'undefined'
      && typeof document.documentElement.clientWidth != 'undefined'
      && document.documentElement.clientWidth != 0)
  {
    size = [ document.documentElement.clientWidth, document.documentElement.clientHeight ];
  }
  else
  {
    size = [ document.getElementsByTagName('body')[0].clientWidth, document.getElementsByTagName('body')[0].clientHeight ];
  }
  return size;
}




var movingLeft = 0;
var movingTop  = 0;
var movingSteps = 0;
var movingBox = null;
var movingDx = 10;
var movingDy = 10;
var movingNrSteps = 20;
var movingText = "";
function startExpressionAnimation(id,text)
{
  movingText = text;
  if (movingBox)
  {
    document.getElementsByTagName("body")[0].removeChild(movingBox);
    movingBox = null;
  }

  var elem = document.getElementById(id);
  var tg = document.getElementById("consoleApplet");
  var isTutorial = false;
  if (tg == null)
  {
    isTutorial = true;
    tg = parent.document.getElementById("consoleApplet");
  }
  if (elem != null && tg != null)
  {
    var elemWidth = elem.offsetWidth;
    var cursorPosition = [0, 0];
    while (elem != null)
    {
      cursorPosition[0] += elem.offsetLeft;
      cursorPosition[1] += elem.offsetTop;
      elem = elem.offsetParent;
    }

    var targetPosition = [0, 0];
    if (isTutorial)
    {
      targetPosition[0] = cursorPosition[0];
      targetPosition[1] = cursorPosition[1]-224;
    }
    else
    {
      targetPosition[0] += tg.offsetWidth;
      targetPosition[0] -= elemWidth;
      targetPosition[1] += tg.offsetHeight;
      targetPosition[1] -= 10;
      while (tg != null)
      {
        targetPosition[0] += tg.offsetLeft;
        targetPosition[1] += tg.offsetTop;
        targetPosition[1] -= 20;
        tg = tg.offsetParent;
      }
    }
    movingLeft = cursorPosition[0];
    movingTop  = cursorPosition[1];
    movingSteps = 0;
    movingBox = document.createElement("div");
    movingBox.innerHTML = text;
    movingBox.style.padding = "2px 2px 2px 2px;";
    movingBox.style.color = "#000000";
    movingBox.style.position = "absolute";
    movingBox.style.left = movingLeft + "px";
    movingBox.style.top = movingTop + "px";

    movingDx = (targetPosition[0] - cursorPosition[0])/movingNrSteps;
    movingDy = (targetPosition[1] - cursorPosition[1])/movingNrSteps;

    document.getElementsByTagName("body")[0].appendChild(movingBox);
    setTimeout("animateExpressionMover()",10);
  }
  else
  {
    yacasEval(text);
  }
}

function animateExpressionMover()
{
  if (movingSteps == movingNrSteps)
  {
    document.getElementsByTagName("body")[0].removeChild(movingBox);
    movingBox = null;
//    yacasEval(movingText);

    var consoleApplet = getConsoleApplet();
    if (consoleApplet)
    {
      var expression = getPlainText(movingText);
      consoleApplet.InvokeCalculationSilent(expression);
    }
  }
  else
  {
    if (movingSteps == movingNrSteps-5)
    {
      var consoleApplet = getConsoleApplet();
      if (consoleApplet)
      {
        expression = getPlainText(movingText);
        consoleApplet.AddInputLine(expression);
      }
    }

    movingSteps += 1;
    movingLeft += movingDx;
    movingTop  += movingDy;
    movingBox.style.left = movingLeft + "px";
    movingBox.style.top = movingTop + "px";
    setTimeout("animateExpressionMover()",10);
  }
}



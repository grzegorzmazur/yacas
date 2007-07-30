
// This file has some functions needed in pages where commands need to be sent to a Java applet.


var prevOnLoadTutorial = window.onload;
window.onload = initPage;

function initPage()
{
  if (prevOnLoadTutorial)
    prevOnLoadTutorial();

  if (!document.getElementsByTagName)
  { 
    return; 
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
    var links = document.getElementsByTagName("span");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "codeEdit")
        {
          object.innerHTML = 
'      <table cellspacing=0>' +
'        <tr>' +
'          <td style="border-color:black; border-style:solid; border-width:thin; border-bottom:0px; background-color: #DDDDEE;">' +
'          <span>Code editor<\/span>' +
'          <\/td>' +
'        <\/tr>' +
'        <tr>' +
'          <td style="border-color:black; border-style:solid; border-width:thin; background-color:#fafabe;">' +
'            <textarea style="background-color:#fafabe;" class="codeEditor" id="codeText"><\/textarea>' +
'          <\/td>' +
'        <\/tr>' +
'      <\/table>';

        }
        else if (object.className == "yacasConsole")
        {
          var width = 750;
          var height = 400;
          var jarFile = "yacas.jar";
          var programMode = "console";
          if (object.id)
          {
            programMode = object.id;
            if (object.id == "tutorial")
            {
              height = 200;
            }
          }
          object.innerHTML = 

'    <table cellspacing=0>' + 
'      <tr>' + 
'        <td style="border-color:black; border-style:solid; border-width:thin; border-bottom:0px; background-color: #DDDDEE;">' + 
'        <span>Yacas calculation environment<\/span>' + 
'        <\/td>' + 
'      <\/tr>' + 
'      <tr>' + 
'        <td style="border-color:black; border-style:solid; border-width:thin;">' + 
'          <APPLET id="consoleApplet" name="consoleApplet" archive="yacas.jar" code="net.sf.yacas.ConsoleApplet" width='+width+' height='+height+' alt="Java support seems to be disabled in your browser, so the console is not available. If you want to be able to use Yacas online, please enable Java support. Alternatively, you can go to the About Yacas tab to download Yacas and run it locally.">' + 
'            <PARAM name="programMode" value="'+programMode+'"' + 
'            <PARAM name="progressbar" value="true" \/>' + 
'            <PARAM name="boxmessage" value="Loading Yacas..." \/>' + 
'            <PARAM NAME="init1" value="Load(\'\'yacasinit.ys\'\')" \/>' + 
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



  {
    var links = document.getElementsByTagName("textarea");
    for (var i=0; i<links.length; i++)
    {
      var object = links[i];
      if (object.className)
      {
        if (object.className == "codeEditor")
        {
          var datahub = getDatahub();

          if (datahub)
          {
            try
            {
              object.value = datahub.getProgramToLoad();
            }
            catch (e) 
            {
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
  if (parent.document.consoleApplet)
  {
    parent.document.consoleApplet.InvokeCalculation(getPlainText(this.innerHTML));
  }
  else if (document.consoleApplet)
  {
    document.consoleApplet.InvokeCalculation(getPlainText(this.innerHTML));
  }
}

function yacasEval(expression)
{
  if (parent.document.consoleApplet)
  {
    parent.document.consoleApplet.InvokeCalculation(getPlainText(expression));
  }
  else if (document.consoleApplet)
  {
    document.consoleApplet.InvokeCalculation(getPlainText(expression));
  }
}

function commandEdit(base)
{
  var elem = parent.document.getElementById('ConsoleFrame');
  if (elem)
  {
    elem.contentWindow.document.location.href = base+"edit.html"+document.location.search;
  }
}
function commandView(frame,base)
{
  var datahub = getDatahub();
  if (datahub)
  {
    var elem = document.getElementById('codeText');
    if (elem)
    {
      if (datahub)
      {
        try 
        {
          datahub.setProgramToLoad(elem.value);
        } 
        catch (e) 
        {
          alertUpgradeJava();
        }
      }
    }
  }
  {
    var elem = parent.document.getElementById(frame);
    if (elem)
    {
      elem.contentWindow.document.location.href = "journalview.html"+document.location.search;
    }
  }
}


function articleToHTML(text)
{
  text = text.replace(/!!(.+)\n/g,'<h1>$1</h1>');
  text = text.replace(/\n\n/g,"<br /><br />");
  text = text.replace(/\{\{/g,'<table width="100%"><tr><td width=100% bgcolor="#DDDDEE"><pre>');
  text = text.replace(/\}\}/g,'</pre></tr></table>');
  text = text.replace(/\{(.+)\}/g,'<tt><b>$1</b><tt>');
  return text;
}




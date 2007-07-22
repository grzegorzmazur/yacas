
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
        if (object.className == "editexample")
        {
          object.onclick=editExampleLinkClick;
        }
        if (object.className == "tryexample")
        {
          object.onclick=tryExampleLinkClick;
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
          var width = 900;
          var height = 390;
          if (object.id)
          {
            if (object.id == "full")
            {
            }
            else
            {
              width = 450;
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
  this.style.color="#000000";
}

function commandLinkOut()
{
  this.style.color="#AAE";
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
}

function tryExampleLinkClick()
{
  {
    var elem = parent.document.getElementById("theTabs");
    if (elem)
    if (elem.tabber)
    {
      elem.tabber.tabShow(1);
    }
  }

  var elem;
  elem = document.getElementById('ConsoleFrame');
  if (elem == null)
  {
    elem = parent.document.getElementById('TutorialFrame');
  }
  if (elem == null)
  {
    elem = parent.parent.document.getElementById('TutorialFrame');
  }
  if (elem)
  {
    elem.contentWindow.document.location.href = this.id;
  }
}

function editExampleLinkClick()
{
  if (confirm("This operation will throw away a previous program you had written. Is that ok?"))
  {
    var datahub = getDatahub();
    
    if (datahub)
    {
      try 
      {
        var program = datahub.getProgramToLoad();
        datahub.setProgramMode("console");
        datahub.setProgramToLoad(program);
      } 
      catch (e) 
      {
        alert("Something seems to be wrong with Java support, the Yacas calculation center does not seem to be available. You can probably fix this by downloading a newer version of Java for your system from http://www.java.com/ .");
      }
    }

    {
      var elem = parent.document.getElementById("theTabs");
      if (elem)
      if (elem.tabber)
      {
        elem.tabber.tabShow(3);
      }
    }

    var elem;
    elem = document.getElementById('ConsoleFrame');
    if (elem == null)
    {
      elem = parent.document.getElementById('ConsoleFrame');
    }
    if (elem == null)
    {
      elem = parent.parent.document.getElementById('ConsoleFrame');
    }
    if (elem)
    {
      elem.contentWindow.document.location.href = "../consoleedit.html";
    }
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
function commandView(base)
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
        alert("Something seems to be wrong with Java support, the Yacas calculation center does not seem to be available. You can probably fix this by downloading a newer version of Java for your system from http://www.java.com/ .");
        }
      }
    }
  }
  {
    var elem = parent.document.getElementById('ConsoleFrame');
    if (elem)
    {
      elem.contentWindow.document.location.href = base+"view.html"+document.location.search;
    }
  }
}




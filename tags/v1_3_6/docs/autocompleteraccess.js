var prevOnLoadAutocompl = window.onload;
window.onload = initPage;
function initPage()
{
  if (prevOnLoadAutocompl) prevOnLoadAutocompl();
  if (document.getElementById("funcLookup"))
  {
    document.getElementById("funcLookup").onkeyup = searchSuggest;
    setTimeout('updateHints("")',100);
    
  }
}


var highlighted = null;
var tip = null;
function enterItem(id)
{
  id = parseInt(id);

  var elem = document.getElementById(id);
  if (elem == highlighted)
    return;

  if (highlighted)
  {
    highlighted.style.backgroundColor = "#FFFFFF";
  }
  highlighted = elem;

  if (tip)
  {
    document.getElementsByTagName("body")[0].removeChild(tip);
  }

  var exString = "";
  if (hints[id+3] != "")
  {
    if (allowDemos)
    {
      exString = '<hr><h1>Examples</h1><i>(clicking on one of the items below sends them to the Yacas calculation center on the left)</i><ul>';
      var examples = hints[id+3].split("<sep>");
      for (var i=0;i<examples.length;i++)
      {
        exString = exString + '<li><a id="demo'+i+'" href="javascript:startExpressionAnimation(\'demo'+i+'\', unescape(\''+escape(examples[i])+'\'));">'+examples[i]+'</a>';
      }
      exString = exString + "</ul>";
    }
  }


  tip = document.createElement("div");
  tip.className = "autocompletertooltip";
  tip.innerHTML = 
'<b>Function '+hints[id]+'</b> &mdash; '+hints[id+2]+' <a target="newwin" href="ref.html?'+hints[id]+'">more...</a>' +
'<hr>' +
'<h1>Prototypes</h1>'+hints[id+1] +
exString;


  tip.style.position = "absolute";

  var cursorPosition = [0, 0];


  if (elem != null)
  {
    elem.style.backgroundColor = "#AAAAEE";
    cursorPosition[0] += elem.offsetWidth + 16;
  }
  elem = document.getElementById("popups");
  while (elem != null)
  {
    cursorPosition[0] += elem.offsetLeft;
    cursorPosition[1] += elem.offsetTop;
    elem = elem.offsetParent;
  }

  var left = cursorPosition[0];
  var top = cursorPosition[1];

  tip.style.left = left + "px";
  tip.style.top = top + "px";
  tip.style.visibility = "hidden";
  tip.style.overflow = "hidden";
  document.getElementsByTagName("body")[0].appendChild(tip);

  var viewportSize = getViewportSize();

  if (left > viewportSize[0]-tip.offsetWidth)
  {
    left = viewportSize[0]-tip.offsetWidth;
    if (left < cursorPosition[0]+10-48)
      left = cursorPosition[0]+10-48;
    tip.style.left = left;
  }
  if (top > viewportSize[1]-tip.offsetHeight)
  {
    top = viewportSize[1]-tip.offsetHeight;
    tip.style.top = top;
  }

  tip.style.visibility = "visible";
}



function searchSuggest()
{
  var searchString = this.value;
  updateHints(searchString);
}

var allowDemos = true;

function setAllowDemos(allow)
{
  allowDemos = allow;
}

function updateHints(searchString)
{
  var firstId = "";
  var firstIdIndex = 100;
  if (tip)
  {
    document.getElementsByTagName("body")[0].removeChild(tip);
  }
  highlighted = null;
  tip = null;

  var popups = document.getElementById("popups");

  popups.style.visibility = "hidden";
  popups.innerHTML = "";
  {
    var matches = new Array("","","","","","","","");
    var lengths = new Array(0,0,0,0,0,0,0,0);
    var moreAvailable = 0;
    var maxMatches=1000;//15;

    // Most interesting functions, make sure people see these first so they have something interesting to click on
    var fronts = ":Div:N:Select:Factor:Taylor:D:Integrate:Simplify:Solve:Sum:Limit:DiagonalMatrix:ForEach:Pi:Plot2D:";

    var lwr = searchString.toLowerCase();
    for (var i=0;i<hints.length;i=i+4)
    {
      var exampleAvailable = false;
      if (hints[i+3] != "")
      {
        exampleAvailable = true;
      }

      var line = '<div style="width:148px;" class="suggestions" id="'+i+'" onmouseover=\'enterItem("'+i+'");\'>'+hints[i]+'<\/div>';
      
      var matchIndex = -1;
      if (hints[i].indexOf(searchString) == 0)
      {
        matchIndex = 1;
      }
      else if (hints[i].indexOf(searchString) > 0)
      {
        matchIndex = 2;
      }
      else if (hints[i].toLowerCase().indexOf(lwr) > -1)
      {
        matchIndex = 3;
      }
      if (matchIndex >= 0)
      {
        // Matches, and is one of the important functions? Then put at the beginning.
        if (fronts.indexOf(":"+hints[i]+":") > -1)
        {
          matchIndex = 0;
        }
        // If no example given then the function is probably less important, put at the end.
        if (exampleAvailable == false)
          matchIndex = matchIndex + 4;
        if (lengths[matchIndex] < maxMatches)
        {
          if (matches[matchIndex] == "")
            if (firstIdIndex > matchIndex)
            {
              firstIdIndex = matchIndex;
              firstId = ""+i;
            }
          lengths[matchIndex] = lengths[matchIndex] + 1;
          matches[matchIndex] = matches[matchIndex] + line;
        }
        else
        {
          moreAvailable = 1;
        }
      }
    }

    var totalmatch = "";
    var totalLength = 0;
    for (var i=0;i<matches.length;i++)
    {
      if (totalLength < maxMatches)
      {
        totalmatch = totalmatch+matches[i];
        totalLength = totalLength + lengths[i];
      }
    }
    if (moreAvailable == 1)
    {
      totalmatch = totalmatch + '<div class="suggestions"><i>... (more available, enter part of function name in input field above)<\/i><\/div>';
    }
    popups.innerHTML = totalmatch;
    popups.style.visibility = "visible";
  }
}

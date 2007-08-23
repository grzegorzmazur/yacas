var prevOnLoadAutocompl = window.onload;
window.onload = initPage;
function initPage()
{
  if (prevOnLoadAutocompl) prevOnLoadAutocompl();
  if (document.getElementById("funcLookup"))
  {
    document.getElementById("funcLookup").onkeyup = searchSuggest;
    updateHints("");
  }
}


var lastExampleIndex = 0;
function getYacasExampleOnFunction(index)
{
  var name = hints[index];
  var examples = name.split("<sep>");
  var toUse = examples[lastExampleIndex % examples.length];
  yacasEval(""+toUse);
  lastExampleIndex = lastExampleIndex+1;
}

function searchSuggest()
{
  var searchString = this.value;
  updateHints(searchString);
}
function updateHints(searchString)
{
  var popups = document.getElementById("popups");
  popups.innerHTML = "";
  {
    var matches = new Array("","","","","","","","");
    var lengths = new Array(0,0,0,0,0,0,0,0);
    var moreAvailable = 0;
    var maxMatches=13;

    // Most interesting functions, make sure people see these first so they have something interesting to click on
    var fronts = ":Div:N:Select:Factor:Taylor:D:Integrate:Simplify:Solve:Limit:DiagonalMatrix:ForEach:Pi:";

    var lwr = searchString.toLowerCase();
    for (var i=0;i<hints.length;i=i+4)
    {
      var exam = "";
      if (hints[i+3] != "")
      {
        exam = '|   <a href="javascript:getYacasExampleOnFunction('+(i+3)+')">Demo<\/a>';
      }
      var line = 
        '<div class="suggestions" title="'+hints[i+1]+'">'+
           '<b>'+hints[i]+'</b> - '+hints[i+2]+
           '<br><a target="newwin" href="ref.html?'+hints[i]+'">Manual<\/a>' + 
           exam + 
        '<\/div>';
      
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
        if (exam == "")
          matchIndex = matchIndex + 4;
        if (lengths[matchIndex] < maxMatches)
        {
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
  }
}

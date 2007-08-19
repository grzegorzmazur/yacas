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

function getYacasHelpOnFunction(name)
{
  alert("This is work in progress, in the future you will see help and examples on "+name+" here.");
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
    var match1 = "";
    var match2 = "";
    var match3 = "";
    var length1 = 0;
    var length2 = 0;
    var length3 = 0;
    var moreAvailable = 0;

    var maxMatches=20;

    var lwr = searchString.toLowerCase();
    for (var i=0;i<hints.length;i=i+2)
    {
      var line = 
        '<div class="suggestions">'+
           hints[i+1]+
           '<br><a href="javascript:getYacasHelpOnFunction(\''+hints[i]+'\')">more...<\/a>' + 
        '<\/div>';
      if (hints[i].indexOf(searchString) == 0)
      {
        if (length1 < maxMatches)
        {
          length1 = length1 + 1;
          match1 = match1 + line;
        }
        else
        {
          moreAvailable = 1;
          break;
        }
      }
      else if (hints[i].indexOf(searchString) > 0)
      {
        if (length2 < maxMatches)
        {
          length2 = length2 + 1;
          match2 = match2 + line;
        }
        else
        {
          moreAvailable = 1;
        }
      }
      else if (hints[i].toLowerCase().indexOf(lwr) > -1)
      {
        if (length3 < maxMatches)
        {
          length3 = length3 + 1;
          match3 = match3 + line;
        }
        else
        {
          moreAvailable = 1;
        }
      }
    }
    var totalmatch = match1;
    var totalLength = length1;
    if (totalLength < maxMatches)
    {
      totalmatch = totalmatch+match2;
      totalLength = totalLength + length2;
    }
    if (totalLength < maxMatches)
    {
      totalmatch = totalmatch+match3;
      totalLength = totalLength + length3;
    }
    if (moreAvailable == 1)
    {
      totalmatch = totalmatch + '<div class="suggestions"><i>... (more available, enter part of function name in input field above)<\/i><\/div>';
    }
    popups.innerHTML = totalmatch;
  }
}

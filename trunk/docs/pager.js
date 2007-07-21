

  var prevOnLoadPager = window.onload;
  window.onload = initPage;
  function initPage()
  {
    if (prevOnLoadPager)
      prevOnLoadPager();
    initPager();
  }



  var urlBase = "none";
  var endPageIndex = 1;
  var currentPage = 1;

  function initPager()
  {
    var spans = document.getElementsByTagName("span");

    // loop through all span tags
    for (var i=0; i<spans.length; i++)
    {
      var spn = spans[i];

      if (spn.className == "pager" && (spn.id)) 
      {
        if (document.location.search)
        {
          var base;
          var nrPages=5;
          if (document.location.search.indexOf("?") == 0)
          {
            base = document.location.search.substring(1);
            var pos = base.indexOf(":");
            if (pos > -1)
            {
              nrPages = base.substring(pos+1);
              base = base.substring(0,pos);

              spn.id = "journal/"+base;
              spn.innerHTML=""+nrPages;
            }
          }
        }

        endPageIndex = spn.innerHTML;

        var height = 350;
        var buttonSrc = 
'              <tr>' +
'                <td align="left"><a id="prevLink" class="pagerButton">Previous page<\/a><\/td>' +
'                <td align="center"><i><span id="pageNum">Page 1<\/span><\/i><\/td>' +
'                <td align="right"><a id="nextLink" class="pagerButton">Next page<\/a><\/td>' +
'              <\/tr>';


        if (endPageIndex == 1)
        {
          height = height + 30;
          buttonSrc = "";
        }

        spn.innerHTML=
'            <table cellpadding="3">' +
'              <tr>' +
'                <td colspan=3>' +
'                  <iframe frameborder="0" id="pagerText" width="450" height="'+height+'" src="'+spn.id+'1.html"><\/iframe>' +
'                <\/td>' +
'              <\/tr>' +
               buttonSrc +
'            <\/table>';
        currentPage = 1;
        urlBase = spn.id;
        showCurrentPage();
      }
    }

    var links = document.getElementsByTagName("a");

    // loop through all span tags
    for (var i=0; i<links.length; i++)
    {
      var ln = links[i];
      if (ln.className == "pagerButton" && (ln.id)) 
      {
        if (ln.id == 'prevLink')
        {
          ln.onmouseover=pagerButtonLinkOverPrev;
          ln.onmouseout=pagerButtonLinkOutPrev;
          ln.onclick = goPreviousPage;
        }
        else if (ln.id == 'nextLink')
        {
          ln.onmouseover=pagerButtonLinkOverNext;
          ln.onmouseout=pagerButtonLinkOutNext;
          ln.onclick = goNextPage;
        }
      }
    }
  }


  function pagerButtonLinkOverPrev()
  {
    if (currentPage != 1)
    {
      this.style.color="#000000";
    }
  }
  function pagerButtonLinkOverNext()
  {
    if (currentPage != endPageIndex)
    {
      this.style.color="#000000";
    }
  }

  function pagerButtonLinkOutPrev()
  {
    if (currentPage != 1)
    {
      this.style.color="#0000FF";
    }
  }

  function pagerButtonLinkOutNext()
  {
    if (currentPage != endPageIndex)
    {
      this.style.color="#0000FF";
    }
  }

  function goPreviousPage()
  {
    if (currentPage > 1)
    {
      currentPage = currentPage - 1;
      showCurrentPage();
    }
  }

  function goNextPage()
  {
    if (currentPage < endPageIndex)
    {
      currentPage = currentPage + 1;
      showCurrentPage();
    }
  }
  
  function showCurrentPage()
  {
    var elem = document.getElementById('pagerText');
    if (elem)
    {
      elem.contentWindow.document.location.href = urlBase+currentPage+'.html';
    }

    elem = document.getElementById('pageNum');
    if (elem)
    {
      elem.innerHTML="Page "+currentPage+"/"+endPageIndex;
    }

    elem = document.getElementById('prevLink');
    if (elem)
    {
      if (currentPage == 1)
      {
        elem.style.cursor="text";
        elem.style.textDecoration="none";
        elem.style.color="#000000";
      }
      else
      {
        elem.style.cursor="pointer";
        elem.style.textDecoration="underline";
        elem.style.color="#0000FF";
      }
    }
    elem = document.getElementById('nextLink');
    if (elem)
    {
      if (currentPage == endPageIndex)
      {
        elem.style.cursor="text";
        elem.style.textDecoration="none";
        elem.style.color="#000000";
      }
      else
      {
        elem.style.cursor="pointer";
        elem.style.textDecoration="underline";
        elem.style.color="#0000FF";
      }
    }
  }






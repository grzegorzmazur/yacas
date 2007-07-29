
function getDocHeight(doc) 
{
  var docHt = 0, sh, oh;
  if (doc.height) 
    docHt = doc.height;
  else if (doc.body) 
  {
    if (doc.body.scrollHeight) 
      docHt = sh = doc.body.scrollHeight;
    if (doc.body.offsetHeight) 
      docHt = oh = doc.body.offsetHeight;
    if (sh && oh) 
      docHt = Math.max(sh, oh);
  }
  return docHt;
}

function setIframeHeight(iframeName, hgt) 
{
  alert("entered setIframeHeight");

  var iframeEl = document.getElementById(iframeName);
  if ( iframeEl) 
  {
alert("iframeEl");
    iframeEl.style.height = "auto"; // helps resize (for some) if new doc shorter than previous
    var docHt = getDocHeight(document);
    // need to add to height to be sure it will all show


    if (docHt) 
    {
docHt = docHt - 300;
alert("docHt = "+docHt);
      iframeEl.style.height = docHt + "px";
    }
  }
alert("left setIframeHeight");
}

function loadIframe(iframeName, url) 
{
  if ( window.frames[iframeName] ) 
  {
    window.frames[iframeName].location = url;
    return false;
  }
  else 
    return true;
}

function goSetHeight(iframename) 
{
  parent.setIframeHeight(iframename);
}



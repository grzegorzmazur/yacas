
function setCookie(name,value)
{
  var expireDate = (new Date((new Date()).getTime() + 100000*3600000)).toGMTString();
  document.cookie = "["+name+"]="+escape(value)+";expires="+expireDate+";";
}
function getCookie(name,defaultValue)
{
  var result = getCookieEscaped(name,defaultValue);
  if (result != defaultValue)
  {
    result = unescape(result);
  }
  return result;
}

function getCookieEscaped(name,defaultValue)
{
  var cookies = document.cookie.split(";");
  for (var i = 0 ; i < cookies.length ; i++)
  {
    var pair = cookies[i].split("=");
    if (pair[0].indexOf("["+name+"]") != -1)
    {
      return pair[1];
    }
  }
  return defaultValue;
}


function addDatahub()
{
  document.write(' <APPLET name="datahub" archive="yacas.jar" code="net.sf.yacas.DatahubApplet" width=1 height=1></APPLET>');
}

function addDatahubInJournal()
{
  document.write(' <APPLET name="datahub" archive="../yacas.jar" code="net.sf.yacas.DatahubApplet" width=1 height=1></APPLET>');
}


function getDatahub()
{
  return document.datahub;
}



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
  var datahub = document.datahub;
  return datahub;
}

function alertUpgradeJava()
{
  alert("Something seems to be wrong with Java support, the Yacas calculation center does not seem to be available. "+
        "You might need to download a newer version of Java for your system from http://www.java.com/ (Java 1.5 is required).");
}

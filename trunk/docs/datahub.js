
function addDatahub()
{
  document.write(' <APPLET name="datahub" archive="yacas.jar" code="net.sf.yacas.DatahubApplet" width=1 height=1></APPLET>');
}

function getDatahub()
{
  var datahub;
  
  if (parent)
    if (parent.document)
      if (parent.document.all)
        if (parent.document.all.datahub)
          datahub = parent.document.all.datahub;

  if (!datahub)
    if (parent)
      if (parent.document)
        if (parent.document.datahub)
          datahub = parent.document.datahub;

  if (!datahub)
    if (document.datahub)
      datahub = document.datahub;
  return datahub;  
}
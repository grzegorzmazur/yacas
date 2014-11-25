package net.sf.yacas;

/** LispDefFile represents one file that can be loaded just-in-time.
 */
class LispDefFile
{
    public LispDefFile(String aFile)
    {
      iFileName = aFile;
      iIsLoaded = false;
    }
    public LispDefFile(LispDefFile aOther)
    {
      iFileName = aOther.iFileName;
      iIsLoaded = aOther.iIsLoaded;
    }
    public void SetLoaded()
    {
      iIsLoaded = true;
    }
    public boolean IsLoaded()
    {
      return iIsLoaded;
    }
    public String FileName()
    {
      return iFileName;
    }

    String iFileName;
    boolean   iIsLoaded;
};

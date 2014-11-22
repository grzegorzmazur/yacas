package net.sf.yacas;


class LispDefFiles extends LispAssociatedHash // <LispDefFile>
{
  LispDefFile File(String aFileName)
  {
    // Create a new entry
    LispDefFile file = (LispDefFile)LookUp(aFileName);
    if (file == null)
    {
      LispDefFile newfile = new LispDefFile(aFileName);
      // Add the new entry to the hash table
      SetAssociation(newfile, aFileName);
      file = (LispDefFile)LookUp(aFileName);
    }
    return file;
  }
}

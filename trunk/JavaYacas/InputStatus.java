
class InputStatus
{

    public InputStatus()
	{ 
	  iFileName = "none";
	  iLineNumber = -1;
	}
    public void SetTo(String aFileName)
	{
	  iFileName = aFileName;
	}
    public void RestoreFrom(InputStatus aPreviousStatus)
	{
	  iFileName = aPreviousStatus.iFileName;
	  iLineNumber = aPreviousStatus.iLineNumber;
	}
    public int LineNumber()
	{
		return iLineNumber;
	}
    public String FileName()
	{
		return iFileName;
	}
    public  void NextLine()
	{
		iLineNumber++;
	}
    String iFileName;
    int	iLineNumber;
};


// epocmain.CPP
//
// yacas console exe for EPOC32.
//
// files belonging to this distro:
//
// epocmain.cpp, yacas.mmp
//
// additionally: epocfileio.cpp and the plat/epoc32 dir
//
// TODO:
//   - arrayclass: constructing LispPtr array with initial value
//     will not work on epoc gcc.
//   - InternalFindFile for epoc.
//   - Send methods should return a value.
//   - epoc version of InputStatus



#include <e32cons.h>
#include "yacas.h"
#include "commandline.h"
#include "standard.h"
#include "ramdisk.h" //TODO keep this?
CYacas* yacas=NULL;

CConsoleBase* console;


class CEpocCommandLine : public CCommandLine
{
public:
    CEpocCommandLine();
    ~CEpocCommandLine();
public:
    virtual LispInt GetKey();
    virtual void NewLine();
    virtual void ShowLine(LispCharPtr prompt,LispInt promptlen,LispInt cursor);
    virtual void Pause();
};

#define KHistoryFile   _L("yacashistory")
CEpocCommandLine::CEpocCommandLine()
{
  RFs fs;
  fs.Connect();
  RFile hist;
  if (hist.Open(fs,KHistoryFile,EFileRead) == KErrNone)
  {
    TFileText text;
    text.Set(hist);

    TBuf<500> line;
    line = _L("  ");
    while (line.Length() > 0)
    {
      text.Read(line);
      line.ZeroTerminate();
      if (line.Length() > 0)
      {
        LispStringPtr string = new LispString();
        (*string) = (char*)&line[0];
        iHistory.Append(string);
      }
    }
    
    hist.Close();
  }
    history=iHistory.NrItems();

}
CEpocCommandLine::~CEpocCommandLine()
{
  RFs fs;
  fs.Connect();
  fs.Delete(KHistoryFile);
  RFile hist;
  if (hist.Create(fs,KHistoryFile,EFileStreamText|EFileWrite) == KErrNone)
  {
    TFileText text;
    text.Set(hist);
    TInt i;
    for (i=0;i<iHistory.NrItems();i++)
    {
      TInt length = iHistory[i]->NrItems()-1;
      TPtr ptr((unsigned char*)iHistory[i]->String(),length,length);
      text.Write(ptr);
    }
    hist.Close();
  }
  
}

LispInt CEpocCommandLine::GetKey()
{
  TInt c = console->Getch();
  switch (c)
  {
  case 8: //  8   backspace
    return eBackSpace;
  case 9: //  9   tab 
      return eTab;
  case 13: // 13   enter
    return eEnter;
  case 127: //127   delete
    return eDelete;
  case 4098://4098 home
    return eHome;
  case 4099://4099 end
    return eEnd;
  case 4103://4103 left
    return eLeft;
  case 4104://4104 right
    return eRight;
  case 4105://4105 up
    return eUp;
  case 4106://4106 down
    return eDown;
  case 27://27 escape
    return eEscape;
  }

  return c;
}

void CEpocCommandLine::NewLine()
{
    console->Printf(_L("\n"));
}

void CEpocCommandLine::ShowLine(LispCharPtr prompt,LispInt promptlen,LispInt cursor)
{
    if (iFullLineDirty)
    {
        console->SetPos(0);
        console->ClearToEndOfLine();
        console->Printf(_L("%s%s"),prompt,&iSubLine[0]);
    }
    console->SetPos(cursor+promptlen);
    iFullLineDirty = 0;
}

void CEpocCommandLine::Pause()
{
}

void DriveEngineL();
void SetupConsoleL();

GLDEF_C TInt E32Main() // main function called by E32
    {
  //There is *sometines* a memory leak. Not at construction though...
//	__UHEAP_MARK;
	CTrapCleanup* cleanup=CTrapCleanup::New(); // get clean-up stack
	TRAPD(error,SetupConsoleL()); // more initialization, then do example
	__ASSERT_ALWAYS(!error,User::Panic(_L("Yacas"),error));
	delete cleanup; // destroy clean-up stack
//	__UHEAP_MARKEND;
	return 0; // and return
    }

void SetupConsoleL() // initialize and call example code under cleanup stack
    {
	//constant declarations
#define KConsoleTitle	_L("Yacas console")
#define KFailedLeaveCode	_L("failed: leave code=%d")
#define KTextOK	_L("ok")

	CActiveScheduler* scheduler=new CActiveScheduler;
	CActiveScheduler::Install(scheduler);

	// setup console code
	console=Console::NewL(KConsoleTitle,
		TSize(KDefaultConsWidth,KDefaultConsHeight));
	CleanupStack::PushL(console);
	TRAPD(error,DriveEngineL()); // perform example function
	if (error) console->Printf(KFailedLeaveCode, error);
	else console->Printf(KTextOK);
	CleanupStack::PopAndDestroy(); // close console
  delete scheduler;
    }

/*
	ok, real stuff starts here
*/

// important codes:
//  8   backspace
//  9   tab 
// 13   enter
//127   delete
//4098 home
//4099 end
//4103 left
//4104 right
//4105 up
//4106 down


TInt busy=1;
void LispExit(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
  busy=LispFalse;
  InternalTrue(aEnvironment, aResult);
}


void ShowResult(char *prompt)
{
  if (yacas->Error()[0] != '\0')
  {
    console->Printf(_L("%s\r\n"),yacas->Error());
  }
  else
  {
    if ((*yacas)().output.iString.String()[0])
    {
      console->Write(_L(&(*yacas)().output.iString.String()[0]));
      (*yacas)().output.iString.SetNrItems(0);
      (*yacas)().output.iString.Append('\0');
    }
    console->Write(_L(prompt));
    console->Write(_L(yacas->Result()));
    console->Write(_L("\r\n"));
  }
}


void DriveEngineL()
{
  TInt line=0;
  CEpocCommandLine cmd;

REDO:
  yacas = CYacas::NewL();

  (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
       (*yacas)()().HashTable().LookUp("Exit"));
  

    yacas->Evaluate("DefaultDirectory(\"c:\\\\Yacas\\\\\");");
    yacas->Evaluate("Load(\"yacasinit\");");
    ShowResult("");

  console->Printf(_L("To exit Yacas, enter  Exit(); Type 'restart' to restart Yacas.\r\n"));
  console->Printf(_L("To see example commands, keep typing Example();\r\n"));

  
  while (busy)
  {

    TBuf<20> prompt;
    prompt.Format(_L("In> "),line);
    prompt.ZeroTerminate();    
    cmd.ReadLine((char*)&prompt[0]);

    if (cmd.iLine == "restart")
    {
      delete yacas;
      goto REDO;
    }
    
    yacas->Evaluate(cmd.iLine.String());
    prompt.Format(_L("Out> "),line);
    prompt.ZeroTerminate();    
    ShowResult((char*)&prompt[0]);
    line++;
  }
  delete yacas;
}

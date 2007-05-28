// epocmain.CPP
//
// TODO we should move this file to a platform-dependent directory.
// yacas console exe for EPOC32.
//
// files belonging to this distro:
//
// epocmain.cpp, yacas.mmp
//
// additionally: epocfileio.cpp and the plat/epoc32 dir
//
// items to do (is some one still maintaining the EPOC32 version?):
//   - arrayclass: constructing LispPtr array with initial value
//     will not work on epoc gcc.
//   - InternalFindFile for epoc.
//   - Send methods should return a value.
//   - epoc version of InputStatus



#include <e32cons.h>
#include <stdio.h> //stdio.h is needed to read the scripts.dat file, which is currently done through fopen/fread/fclose. It is not standard EPOC32, and should perhaps be removed.
#include "yacas.h"
#include "commandline.h"
#include "standard.h"

#include "GPL_stuff.h"

CYacas* yacas=NULL;

CConsoleBase* console;
LispString *the_out = NULL;


class CEpocCommandLine : public CCommandLine
{
public:
    CEpocCommandLine();
    ~CEpocCommandLine();
public:
    virtual LispInt GetKey();
    virtual void NewLine();
    virtual void ShowLine(LispChar * prompt,LispInt promptlen,LispInt cursor);
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
        LispString * string = new LispString();
        (*string) = (char*)&line[0];
        iHistory.Append(string);
      }
    }
 
    hist.Close();
  }
    history=iHistory.Size();

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
    for (i=0;i<iHistory.Size();i++)
    {
      TInt length = iHistory[i]->Size()-1;
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
    console->Write(_L("\n"));
}

void CEpocCommandLine::ShowLine(LispChar * prompt,LispInt promptlen,LispInt cursor)
{
    if (iFullLineDirty)
    {
        console->SetPos(0);
        console->ClearToEndOfLine();
        TBuf<200> p = (unsigned char*)prompt;
        console->Write(p);
        p = (unsigned char*)&iSubLine[0];
        console->Write(p);
        //hier        console->Printf(_L("%s%s"),prompt,&iSubLine[0]);
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
//  __UHEAP_MARK;
  CTrapCleanup* cleanup=CTrapCleanup::New(); // get clean-up stack
  TRAPD(error,SetupConsoleL()); // more initialization, then do example
  __ASSERT_ALWAYS(!error,User::Panic(_L("Yacas"),error));
  delete cleanup; // destroy clean-up stack
//  __UHEAP_MARKEND;
  return 0; // and return
    }

void SetupConsoleL() // initialize and call example code under cleanup stack
    {
  //constant declarations
#define KConsoleTitle  _L("Yacas console")
#define KFailedLeaveCode  _L("failed: leave code=%d")
#define KTextOK  _L("ok")

  CActiveScheduler* scheduler=new CActiveScheduler;
  CActiveScheduler::Install(scheduler);

  // setup console code
  console=Console::NewL(KConsoleTitle,
    TSize(KDefaultConsWidth,KDefaultConsHeight));
  CleanupStack::PushL(console);
  TRAPD(error,DriveEngineL()); // perform example function
  if (error)
  {
//hier    console->Printf(KFailedLeaveCode, error);
  }
  else
  {
//hier    console->Printf(KTextOK);
  }
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
    TBuf<100> error = (unsigned char*)yacas->Error();
    console->Write(error);
    //hier    console->Printf(_L("%s\r\n"),yacas->Error());
  }
  else
  {
    if (the_out->String()[0])
    {
      console->Write(_L(&the_out->String()[0]));
      the_out->Resize(0);
      the_out->Append('\0');
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

  the_out = NEW LispString;
REDO:
  yacas = CYacas::NewL(new StringOutput(*the_out));

  (*yacas)()().Commands().SetAssociation(LispEvaluator(LispExit),
       (*yacas)()().HashTable().LookUp("Exit"));
 

//    yacas->Evaluate("DefaultDirectory(\"c:\\\\Yacas\\\\\");");


  {
    FILE*fin = fopen("scripts.dat","rb");
    if (!fin)
    {
        console->Write(_L("Error, could not open archive file\n"));
    }
    else
    {
      fseek(fin,0,SEEK_END);
      int fullsize = ftell(fin);
      fseek(fin,0,SEEK_SET);
      unsigned char* fullbuf = (unsigned char*)PlatAlloc(fullsize);
      if (fullbuf)
      {
        fread(fullbuf,1,fullsize,fin);
        CCompressedArchive *a =
            NEW CCompressedArchive(fullbuf, fullsize, 1);
        if (a->iFiles.IsValid())
        {
            (*yacas)()().iArchive = a;
        }
        else
        {
            console->Write(_L("Error, is not a valid archive file.\n"));
            delete a;
        }
      }
      else
      {
          console->Write(_L("Archive file too large, perhaps it is time we\nimplement disk-accessed compressed files.\n"));
      }
      fclose(fin);
    }
  }


    yacas->Evaluate("Load(\"yacasinit.ys\");");
    ShowResult("");

// do we have a VERSION define on epoc?
//  console->Write(_L("This is Yacas version '" VERSION "'.\n"));
  console->Write(_L(GPL_blurb_nohelp));

  console->Write(_L("To exit Yacas, enter  Exit(); Type 'restart' to restart Yacas.\r\n"));
  console->Write(_L("To see example commands, keep typing Example();\r\n"));

 
  while (busy)
  {

    TBuf<20> prompt;
    prompt = _L("In> ");
    prompt.ZeroTerminate();
    cmd.ReadLine((char*)&prompt[0]);

    if (cmd.iLine == "restart")
    {
      delete yacas;
      goto REDO;
    }
 
    yacas->Evaluate(cmd.iLine.String());
    prompt = _L("Out> ");
    prompt.ZeroTerminate();
    ShowResult((char*)&prompt[0]);
    line++;
  }
  delete yacas;
}


//TODO move to a separate platform-dependent directory?

#include "epocclsv.h"
#include "epocserv.h"

#include <stdio.h> //stdio.h is needed to read the scripts.dat file, which is currently done through fopen/fread/fclose. Non-standard EPOC32, current maintainer should have a look at this.

//BEGIN DEBUG
//RFs fs;
//RFile f;

#if defined(__WINS__)
  #define debugprintf(s)
#else
  #define debugprintf(s)  //f.Write(_L(s))
#endif
//END DEBUG

//**********************************
//CYacasServer
//**********************************


CYacasServer::CYacasServer(TInt aPriority)
  : CServer(aPriority)
{
  __DECLARE_NAME(_S("CYacasServer"));
}

void CYacasServer::RunL()
{
  CServer::RunL();
}

// Create and start a new count server.
CYacasServer* CYacasServer::New()
{
// debugprintf("1...r\n");
  CYacasServer *pS=new CYacasServer(EPriority);
  __ASSERT_ALWAYS(pS!=NULL,PanicServer(ESvrCreateServer));
// debugprintf("2...r\n");
  HBufC *pN=(&KCountServerName)->Alloc();
  __ASSERT_ALWAYS(pN!=NULL,PanicServer(ESvrCreateServer));
// debugprintf("3...r\n");
  pS->iName=pN;
  TInt r=pS->Start();
// debugprintf("4...r\n");
  // if the server was already started, we can ignore this, and just leave
  if (r==KErrAlreadyExists)
  {
    // debugprintf("r=KErrAlreadyExists, exiting\n");
    User::Exit(KErrNone);
  }

  __ASSERT_ALWAYS(r==KErrNone,PanicServer(ESvrStartServer));
// debugprintf("5...r\n");
  return pS;
}

// Create a new server session.
CSession *CYacasServer::NewSessionL(RThread aClient, const TVersion &aVersion) const
{
  // check we're the right version
  TVersion v(KCountServMajorVersionNumber,KCountServMinorVersionNumber,KCountServBuildVersionNumber);
  if (!User::QueryVersionSupported(v,aVersion))
    User::Leave(KErrNotSupported);
  // make new session
  return CYacasServerSession::NewL(aClient, (CYacasServer*)this);
}

//**********************************
//CYacasServerSession
//**********************************


// constructor - must pass client to CSession
CYacasServerSession::CYacasServerSession(RThread &aClient, CYacasServer * aServer)
  : CSession(aClient)
{
  __DECLARE_NAME(_S("CYacasServerSession"));
  iCountSvr=aServer;
}

CYacasServerSession* CYacasServerSession::NewL(RThread &aClient, CYacasServer * aServer)
{
  return new(ELeave) CYacasServerSession(aClient,aServer);
}


void CYacasServerSession::ServiceL(const RMessage& aMessage)
{
  TRAPD(err,DispatchMessageL(aMessage));
  aMessage.Complete(err);
}

// service a client request; test the opcode and then do appropriate servicing
void CYacasServerSession::DispatchMessageL(const RMessage &aMessage)
{
  switch (aMessage.Function())
  {
    case EYacasServStart:
      // debugprintf("EYacasServStart received\r\n");
      ConstructYacas();
      return;
    case EYacasServStop:
      // debugprintf("EYacasServStop received\r\n");
      DeleteYacas();
      return;
    case EYacasServSendCommand:
      // debugprintf("EYacasServSendCommand received\r\n");
      Evaluate();
      return;
    // requests we don't understand at all are a different thing,
    // so panic the client here, this function also completes the message
    default:
      PanicClient(EBadRequest);
      return;
  }
}

// Evaluate string from the client and return the result
void CYacasServerSession::Evaluate()
{
  TCalculation calc;
  ReadL(Message().Ptr0(),calc);
  TCalculation result;

  #ifdef HAS_YACAS
  iYacas->Evaluate((char*)calc().text.Ptr());
  if (iYacas->Error()[0] != '\0')
  {
    result().text.Format(_L("%s\r\n"),iYacas->Error());
  }
  else
  {
    if (theOutput->iString.String()[0])
    {
      result().text.Format(_L("%s\n%s\r\n"),
                           &theOutput->iString.String()[0],
                           iYacas->Result());
      theOutput->iString.Resize(0);
      theOutput->iString.Append('\0');
    }
    else
    {
      result().text.Format(_L("%s\r\n"),iYacas->Result());
    }
  }
  #endif

  result().text.ZeroTerminate();
  Write(Message().Ptr0(),result);

  TInt res;
  const TAny* pD=Message().Ptr0();
  TInt desLen=Message().Client().GetDesLength(pD);
  HBufC8* writeBuf=HBufC8::New(desLen);
  TPtr initptr = writeBuf->Des();
  // read the contents of the client pointer into a TPtr.
  TRAP(res,Message().ReadL(pD,initptr));
  if (res!=KErrNone)
    PanicClient(EBadDescriptor);
}

// panic the client
void CYacasServerSession::PanicClient(TInt aPanic) const
{
  _LIT(KTxtServer,"YacasServ server");
  Panic(KTxtServer,aPanic);
}

// write to the client thread; if unsuccessful, panic the client
void CYacasServerSession::Write(const TAny* aPtr,const TDesC8& aDes,TInt anOffset)
{
  TRAPD(ret,WriteL(aPtr,aDes,anOffset);)
  if (ret!=KErrNone)
    PanicClient(EBadDescriptor);
}

void CYacasServerSession::ConstructYacas()
{
  // debugprintf("5...Constructing Yacas\n");
  theOutput = new StringOutput(stringOutput);
  iYacas = CYacas::NewL(theOutput);

  FILE*fin;
  fin = fopen("C:\\System\\Apps\\Yacas\\Scripts.dat","rb");
  if (!fin)
  {
    fin = fopen("D:\\System\\Apps\\Yacas\\Scripts.dat","rb");
    if (!fin)
    {
      fin = fopen("Z:\\System\\Apps\\Yacas\\Scripts.dat","rb");
      if (!fin)
      {
        // return error message
        // iConsole->Printf(_L("Error, could not open archive file\n"));
      }
    }
  }
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
        (*iYacas)()().iArchive = a;
    }
    else
    {
        // iConsole->Printf(_L("Error, is not a valid archive file.\n"));
        delete a;
    }
  }
  else
  {
     // iConsole->Printf(_L("Archive file too large, perhaps it is time we\nimplement disk-accessed compressed files.\n"));
  }
  fclose(fin);

  // debugprintf("7...Evaluating yacasinit.ys\n");

  iYacas->Evaluate("Load(\"yacasinit.ys\");");
}

void CYacasServerSession::DeleteYacas()
  {
#ifdef HAS_YACAS
  delete iYacas;
#endif
#if defined(__WINS__)
#else
  CActiveScheduler::Stop();
#endif
  }

//**********************************
//Global functions
//**********************************


// The count server thread.


#if defined(__WINS__)
GLDEF_C TInt E32Dll(TDllReason)
{
  return(KErrNone);
}


// The count server thread.
GLDEF_C TInt CYacasServer::ThreadFunction(TAny* anArg)
{

  // convert argument into semaphore reference
  RSemaphore& semaphore=*(RSemaphore *)anArg;

  // start scheduler and server
  CActiveScheduler *pA=new CActiveScheduler;
  __ASSERT_ALWAYS(pA!=NULL,PanicServer(EMainSchedulerError));
  CActiveScheduler::Install(pA);
  CYacasServer::New();
  // signal that we've started
  semaphore.Signal();
  // start fielding requests from clients
  CActiveScheduler::Start();
  // finished
  return(KErrNone);
}

// Create the server thread
// This function is exported from the DLL and called from the client
EXPORT_C TInt StartThread()
{
  TInt res=KErrNone;
  // create server - if one of this name does not already exist
  TFindServer findCountServer(KCountServerName);
  TFullName name;
  if (findCountServer.Next(name)!=KErrNone) // we don't exist already
    {
    RThread thread;
    RSemaphore semaphore;
    semaphore.CreateLocal(0); // create a semaphore so we know when thread finished
        res=thread.Create(KCountServerName,   // create new server thread
        CYacasServer::ThreadFunction, // thread's main function
        800000, //KDefaultStackSize,
        3145728, //KDefaultHeapSize,
        3145728, //KDefaultHeapSize,
        &semaphore // passed as TAny* argument to thread function
      );

    if (res==KErrNone) // thread created ok - now start it going
      {
      thread.SetPriority(EPriorityNormal);
      thread.Resume(); // start it going
      semaphore.Wait(); // wait until it's initialized
      thread.Close(); // we're no longer interested in the other thread
      }
    else // thread not created ok
      {
      thread.Close(); // therefore we've no further interest in it
      }

    semaphore.Close();
    }
  // // notify the kernel that a server has started.
  UserSvr::ServerStarted();

  return res;
}

#else // MARM code follows

GLDEF_C TInt E32Main()
{
  CActiveScheduler *pA=new CActiveScheduler;
  __ASSERT_ALWAYS(pA!=NULL,PanicServer(EMainSchedulerError));
  CActiveScheduler::Install(pA);
  CYacasServer *server = CYacasServer::New();
  // signal that we've started
  CActiveScheduler::Start();
  return 0;
}
#endif

GLDEF_C void PanicServer(TCountServPanic aPanic)
{
  _LIT(KTxtServerPanic,"Yacas server panic");
  User::Panic(KTxtServerPanic,aPanic);
}

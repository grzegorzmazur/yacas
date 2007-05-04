// epocserv.h

#if !defined(__EPOCSERV_H__)
#define __EPOCSERV_H__

#include <e32base.h>
#include "yacasprivate.h"
#include "yacas.h"
#include "standard.h"
#include <coeutils.h>

#define HAS_YACAS

// needed for creating server thread.
const TUint KDefaultHeapSize=0x10000;

// reasons for server panic
enum TCountServPanic
{
  EBadRequest,
  EBadDescriptor,
  EMainSchedulerError,
  ESvrCreateServer,
  ESvrStartServer,
  ECreateTrapCleanup,
  ENotImplementedYet,
};


//**********************************
//CYacasServer
//**********************************
//The server class; an active object.
//Contains an instance of RServer; a handle to the kernel server representation
//which is used to receive messages.

class CYacasServer : public CServer
{
public:
  enum {EPriority=950}; // mpt - need to explain the magic here!
public:
  static CYacasServer* New();
  virtual CSession *NewSessionL(RThread aClient,const TVersion &aVersion) const;
  virtual void RunL();
  static TInt ThreadFunction(TAny* aStarted);
protected:
  CYacasServer(TInt aPriority);
private:
  TInt iActive;
};

//**********************************
//CYacasServerSession
//**********************************
//This class represents a session in the server.
//CSession::Client() returns the client thread.
//Functions are provided to respond appropriately to client messages.


class CYacasServerSession : public CSession
{
private:
  void ConstructYacas();
  void DeleteYacas();
public:
  // construct/destruct
  CYacasServerSession(RThread &aClient, CYacasServer * aServer);
  static CYacasServerSession* NewL(RThread &aClient, CYacasServer * aServer);
  // service request
  virtual void ServiceL(const RMessage &aMessage);
  void DispatchMessageL(const RMessage &aMessage);

  // services available to initialize/increase/decrease/reset and return the
  // counter value.
  void Evaluate();
protected:
  // panic the client
  void PanicClient(TInt aPanic) const;
  // safewrite between client and server
  void Write(const TAny* aPtr,const TDesC8& aDes,TInt anOffset=0);
private:
  CYacasServer *iCountSvr;
//  TInt iCount;
  LispString stringOutput;
  StringOutput *theOutput;
#ifdef HAS_YACAS
  CYacas* iYacas;
#endif
};



//**********************************
//global functions
//**********************************

// function to panic the server
GLREF_C void PanicServer(TCountServPanic aPanic);
// thread function for server
GLREF_C TInt CountServerThread(TAny *);

#endif

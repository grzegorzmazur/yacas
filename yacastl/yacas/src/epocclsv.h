// epocclsv.h
//

#if !defined(__EPOCCLSV_H__)
#define __EPOCCLSV_H__

#include <e32base.h>
#include <e32svr.h>
#include <e32math.h>
#include <e32uid.h>
#include <f32file.h>

// server name
_LIT(KCountServerName,"YacasServer");

// A version must be specified when creating a session with the server
const TUint KCountServMajorVersionNumber=0;
const TUint KCountServMinorVersionNumber=1;
const TUint KCountServBuildVersionNumber=1;

#define KMaxYacasStringLength 256
struct TCalculationStruct
{
  TBuf<KMaxYacasStringLength> text;
};

typedef TPckgBuf<TCalculationStruct> TCalculation;


IMPORT_C TInt StartThread();

// opcodes used in message passing between client and server
enum TYacasServRqst
{
  EYacasServStart,
  EYacasServStop,
  EYacasServSendCommand
};

enum TYacasServLeave
{
  ENonNumericString
};

#endif

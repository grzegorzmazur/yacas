// epoccli.h
//

#if !defined(__EPOCCLI_H__)
#define __EPOCCLI_H__

#include <basched.h>
#include <eikenv.h>
#include <coecntrl.h>
#include <coeutils.h>
#include <eikappui.h>
#include <e32keys.h>
#include <eikmenup.h>
#include <eikmenu.hrh>
#include <eikdef.h>
#include <eikconso.h>
#include <eikfontd.h>
#include <eikapp.h>
#include <eikdoc.h>
#include <e32base.h>
#include <e32cons.h>
#include <eikdll.h>
#include <eikon.rsg>
#include <yacas.rsg>

#include "epocyacas.hrh"
#include "commandline.h"
#include "standard.h"

// needed for client interface
#include "epocclsv.h"

// needed for client
#include "epocserv.h"

#if defined(_UNICODE)
const TUid KUidYacasApp = { 1527 } ;
#else
const TUid KUidYacasApp = { 1227 } ;
#endif

#define KHistoryDir    _L("C:\\System\\Apps\\Yacas\\")
#define KHistoryFile   _L("C:\\System\\Apps\\Yacas\\YacasHistory.txt")
#define KYacasRCFile   _L("C:\\System\\Apps\\Yacas\\YacasRC.txt")
#define KMaxHistory 1000
enum TMessageControlFontStyle
{
  EStyleElementBold=EMenuCommandBold,
  EStyleElementItalic=EMenuCommandItalic,
  EStyleElementInverse=EMenuCommandInverse,
  EStyleElementUnderline=EMenuCommandUnderline,
  EStyleElementColor=EMenuCommandColor
};

//
// RYacasSession
//

class RYacasSession : public RSessionBase
{
public:
  RYacasSession();
  TInt Connect();
  TVersion Version() const;
  TInt Stop();
  TInt SetFromString(const TDesC& aString, TBuf<KMaxYacasStringLength>& aResult);
};

//
// CSimpleConsole
//


class CSimpleDocument : public CEikDocument
{
public:
  CSimpleDocument(CEikApplication& aApp): CEikDocument(aApp) { }
private: // from CApaDocument
  CEikAppUi* CreateAppUiL();
};

//
// CConsoleControl
//

class CConsoleControl : public CCoeControl
{
public:
  CConsoleControl(const TDes& aAppFullName) : iAppFullName(aAppFullName){}
  ~CConsoleControl();
  void ConstructL(TInt aFlags);
  void ConstructL(TPoint aLeftTop,const TSize& aSize,TInt aFlags);
  TKeyResponse OfferKeyEventL(const TKeyEvent& aKeyEvent,TEventCode aType);
  void HandlePointerEventL(const TPointerEvent& aPointerEvent);
  void DynInitMenuPaneL(TInt aMenuId,CEikMenuPane* aMenuPane);
  void HandleCommandL(TInt aCommand);
  void ActivateL();

protected:
  void ShowResult(char *prompt);
  void DrawInPrompt();
  void LoadHistory();
  void SaveHistory();
  void LoadYacasRC();

protected:
  void FocusChanged(TDrawNow aDrawNow);
private:
  void ToggleFontStyleAndRedrawL(TMessageControlFontStyle aStyleElement);
  inline TDes& Buf() {return iLine;}
private:
  CEikConsoleScreen* iConsole;
  TInt iAllPrintable,iScrollLock,iIgnoreCursor,iHideCursor;
  TDesC* iSelBufPtr;
  TInt iSmallScreen;
  TInt iHighCursor;
  RYacasSession iYacasSession;

  TFileName iAppFullName;

  TBuf<256> iLine;
  TInt iCursorPos;
  CArrayFixFlat<HBufC*>* iHistory;
  TInt history;
};

//
// CSimpleAppUi
//

class CSimpleAppUi : public CEikAppUi
{
public:
  void ConstructL();
  void CreateConsoleL(TInt aFlags);
  ~CSimpleAppUi();
private: // framework
  void HandleCommandL(TInt aCommand);
  void DynInitMenuPaneL(TInt aMenuId,CEikMenuPane* aMenuPane);
private:
  CConsoleControl* iConsole;
  TInt iBackedUp;
};

#endif

#include "epoccli.h"

//
// Common format strings
//

const TUint kDefaultMessageSlots=4;

//BEGIN DEBUG

#if defined(__WINS__)
  #define debugprintf(s)
#else
  #define debugprintf(s) // f.Write(_L(s))
#endif
//END DEBUG

//**********************************
//RYacasSession
//**********************************

RYacasSession::RYacasSession()
{
}

_LIT(NameC,"c:\\system\\apps\\yacas\\YacasServer.exe");
_LIT(NameD,"d:\\system\\apps\\yacas\\YacasServer.exe");
_LIT(NameZ,"z:\\system\\apps\\yacas\\YacasServer.exe");

// Connect to the  server - default number of message slots = 4
TInt RYacasSession::Connect()
{
#if defined(__WINS__)
  TInt r;
  r=StartThread();
  if (r==KErrNone)
    r=CreateSession(KCountServerName,Version(),kDefaultMessageSlots);
#else
  TInt r=KErrNone;
  TThreadId threadid;
  // load server from correct drive
  if (ConeUtils::FileExists(NameC))
  {
    // debugprintf("Starting C EikDll::StartExeL\n");
    threadid = EikDll::StartExeL(NameC);
  }
  else if (ConeUtils::FileExists(NameD))
  {
    // debugprintf("Starting D EikDll::StartExeL\n");
    threadid = EikDll::StartExeL(NameD);
  }
  else if (ConeUtils::FileExists(NameZ))
  {
    // debugprintf("Starting Z EikDll::StartExeL\n");
    threadid = EikDll::StartExeL(NameZ);
  }
//  else
//    {} // todo - dialog saying cannot find exe
  // debugprintf("Calling CreateSession\n");
  r=CreateSession(KCountServerName,Version(),kDefaultMessageSlots);
#endif

  // start up server
  // debugprintf("Calling SendReceive(EYacasServStart,&p[0])\n");
  TAny *p[KMaxMessageArguments];
  SendReceive(EYacasServStart,&p[0]);
  // debugprintf("Exiting RYacasSession::Connect()\n");
  return(r);
}

// Return the client side version number.
TVersion RYacasSession::Version(void) const
{
  return(TVersion(KCountServMajorVersionNumber,KCountServMinorVersionNumber,KCountServBuildVersionNumber));
}

// send command to the server
TInt RYacasSession::SetFromString(const TDesC& aString, TBuf<KMaxYacasStringLength>& aResult)
{
  TCalculation calc;
  calc().text = aString;
  calc().text.ZeroTerminate();
  TAny *p[KMaxMessageArguments];
  p[0]= (TAny*)(&calc);
  SendReceive(EYacasServSendCommand,&p[0]);
  aResult = calc().text;
  return KErrNone;
}

//stop the server
TInt RYacasSession::Stop()
  {
  TAny *p[KMaxMessageArguments];
  return SendReceive(EYacasServStop,&p[0]);
  }

//
// CConsoleControl
//

void CConsoleControl::LoadHistory()
{
  RFs fs;
  fs.Connect();
  RFile hist;
  if (hist.Open(fs,KHistoryFile,EFileRead) == KErrNone)
  {
    TFileText text;
    text.Set(hist);
    iLine = _L("  ");
    while (iLine.Length() > 0)
    {
      text.Read(iLine);
      iLine.ZeroTerminate();
      if (iLine.Length() > 0)
      {
         HBufC* pB=Buf().Alloc();
        if (pB!=NULL)
        {
          {TRAPD(r,iHistory->AppendL(pB));}
        }
      }
    }
    hist.Close();
  }
  history=iHistory->Count();
}
void CConsoleControl::SaveHistory()
{
  RFs fs;
  fs.Connect();
  fs.Delete(KHistoryFile);
  // make directory for storing history file
  fs.MkDirAll(KHistoryDir);

  RFile hist;
  if (hist.Create(fs,KHistoryFile,EFileStreamText|EFileWrite) == KErrNone)
  {
    TFileText text;
    text.Set(hist);
    TInt i;
  for (i=0;i<history;i++)
    {
      HBufC* pL=(*iHistory)[i];
      Buf()=(*pL);
      text.Write(iLine);
    }
    hist.Close();
  }
}

void CConsoleControl::LoadYacasRC()
{
  TBuf<KMaxYacasStringLength> input;
  TBuf<KMaxYacasStringLength> result;
  TInt ret;
  RFs fs;
  fs.Connect();
  RFile yacasrcfile;
  if (yacasrcfile.Open(fs,KYacasRCFile,EFileRead) == KErrNone)
  {
    iConsole->Printf(_L("Loading "));
    iConsole->Printf(KYacasRCFile);
    iConsole->Printf(_L("\r\n"));
    TFileText text;
    text.Set(yacasrcfile);
    iLine = _L("  ");
    while (iLine.Length() > 0)
    {
      text.Read(iLine);
      iLine.ZeroTerminate();
      if (iLine.Length() > 0)
      {
        // evaluate YacasRC line

        input = iLine.Ptr();
        ret = iYacasSession.SetFromString(input,result);
        iConsole->Printf(_L("Out> "));
        iConsole->Printf(result);
        iConsole->Printf(_L("\r\n"));
//        DrawInPrompt();
      }
    }
    yacasrcfile.Close();
  }
}

CConsoleControl::~CConsoleControl()
{

  TInt ret;
  ret = iYacasSession.Stop();

  _LIT(KTxt1,"Sorry, this function is not supported\n");
  if (ret==KErrNotSupported)
    iConsole->Printf(KTxt1);

  // close the count server session
  iYacasSession.Close();
  SaveHistory();

  // destroy the line editor
  TInt count=iHistory->Count();
  while (count--)
    User::Free((*iHistory)[count]);
  delete iHistory;
  delete iSelBufPtr; // forget selection
  delete iConsole;
}


void CConsoleControl::ConstructL(TInt aFlags)
{
  CreateWindowL();
    Window().SetShadowDisabled(ETrue);
    Window().SetBackgroundColor(KRgbGray);
    EnableDragEvents();
    SetExtentToWholeScreenL();
  SetBlank();


  iConsole=new(ELeave) CEikConsoleScreen;

  iConsole->ConstructL(_L("Yacas"),aFlags);
  iHistory=new(ELeave) CArrayFixFlat<HBufC*>(KMaxHistory+2);
  iConsole->SetHistorySizeL(10,10);
  iLine.Zero();
}

void CConsoleControl::ConstructL(TPoint aTopLeft,const TSize& aSize,TInt aFlags)
{
  CreateWindowL();
  Window().SetShadowDisabled(ETrue);
  Window().SetBackgroundColor(KRgbGray);
  EnableDragEvents();
  SetExtentToWholeScreenL();
  SetBlank();
  LoadHistory();

  iConsole=new(ELeave) CEikConsoleScreen;
  iConsole->ConstructL(_L("Yacas"),aTopLeft,aSize,aFlags,EEikConsWinInChars);
  iConsole->SetHistorySizeL(10,10);
  iLine.Zero();
}

void CConsoleControl::DrawInPrompt()
{
  iConsole->Printf(_L("In> "));

  iCursorPos = 0;
  iConsole->SetPos(iCursorPos+4);
  iConsole->FlushChars();
  iConsole->DrawCursor();
}

void CConsoleControl::ActivateL()
{
  CCoeControl::ActivateL();
  iConsole->SetKeepCursorInSight(TRUE);
  iConsole->SetAtt(4,15);
  iConsole->Printf(_L("Yacas for EPOC32\r\n"));
  iConsole->FlushChars();
  iConsole->DrawCursor();
  iConsole->SetAtt(ATT_NORMAL);
  iConsole->DrawCursor();

  // Connect to the Yacas server
  User::LeaveIfError(iYacasSession.Connect());
  LoadYacasRC();
  LoadHistory();
  DrawInPrompt();
}

void CConsoleControl::DynInitMenuPaneL(TInt aMenuId,CEikMenuPane* aMenuPane)
{
  if (aMenuId==R_CONS_OPTIONS_MENU)
  {
    if ( iConsole->Att() & ATT_COLORMASK )
      aMenuPane->SetItemButtonState(EMenuCommandColor,EEikMenuItemSymbolOn);
    else
      {
      if ( iConsole->Att() & ATT_BOLD )
        aMenuPane->SetItemButtonState(EMenuCommandBold,EEikMenuItemSymbolOn);
      if ( iConsole->Att() & ATT_INVERSE )
        aMenuPane->SetItemButtonState(EMenuCommandInverse,EEikMenuItemSymbolOn);
      if ( iConsole->Att() & ATT_ITALIC )
        aMenuPane->SetItemButtonState(EMenuCommandItalic,EEikMenuItemSymbolOn);
      if ( iConsole->Att() & ATT_UNDERLINE )
        aMenuPane->SetItemButtonState(EMenuCommandUnderline,EEikMenuItemSymbolOn);
    }
  }

  if (aMenuId==R_CONS_SPECIAL_MENU)
  {
    if (iHighCursor)
      aMenuPane->SetItemButtonState(EMenuCursorSize,EEikMenuItemSymbolOn);
    if (iSmallScreen)
      aMenuPane->SetItemButtonState(EMenuScreenSize,EEikMenuItemSymbolOn);
  }

  if (aMenuId==R_CONS_TOOLS_MENU)
  {
    if (iHideCursor)
      aMenuPane->SetItemButtonState(EMenuCommandHideCursor,EEikMenuItemSymbolOn);
    if (iIgnoreCursor)
      aMenuPane->SetItemButtonState(EMenuCommandIgnoreCursor,EEikMenuItemSymbolOn);
    if (iScrollLock)
      aMenuPane->SetItemButtonState(EMenuCommandScrollLock,EEikMenuItemSymbolOn);
    if (iAllPrintable)
      aMenuPane->SetItemButtonState(EMenuCommandPrintable,EEikMenuItemSymbolOn);
  }
}

void CConsoleControl::HandleCommandL(TInt aCommand)
{
  switch (aCommand)
  {
    case EMenuCommandFileExit:
      CBaActiveScheduler::Exit();

    case EMenuCommandEditCopy      :
    {
      TRect range = iConsole->Selection();  // get current selected range
      if (iSelBufPtr) delete iSelBufPtr;    // forget previous selection
      iSelBufPtr = iConsole->RetrieveL(range);
      if (iSelBufPtr)
        {
        TBuf<32> msg;
        msg.Format(_L("%d bytes copied"),iSelBufPtr->Length());
        iEikonEnv->InfoMsg(msg);
        }
      else
        iEikonEnv->InfoMsg(_L("Nothing to copy..."));
    }
    break;
    case EMenuCommandEditPaste      :
      iConsole->SelectCursor(); // forget current selection...
      if (iSelBufPtr)
      {
        iConsole->Write(*iSelBufPtr);
        iConsole->FlushChars();
        TBuf<32> msg;
        msg.Format(_L("%d bytes pasted"),iSelBufPtr->Length());
        iEikonEnv->InfoMsg(msg);
      }
      else
      {
        iEikonEnv->InfoMsg(_L("Nothing to paste..."));
      }
      break;

    case EMenuCommandBold:
    case EMenuCommandItalic:
    case EMenuCommandUnderline:
    case EMenuCommandInverse:
    case EMenuCommandColor:
      ToggleFontStyleAndRedrawL((TMessageControlFontStyle)aCommand);
      break;

    case EMenuScreenSize:
    {
      iSmallScreen = !iSmallScreen;
      if (iSmallScreen)
      {
        iConsole->ConsoleControl()->SetExtentL( TPoint(40,20), TSize(560,200) );
      }
      else
      {
        iConsole->ConsoleControl()->SetExtentToWholeScreenL();
      }
    }
    break;
    case EMenuCursorSize:
    {
      iHighCursor = !iHighCursor;
      if (iHighCursor)
        iConsole->SetCursorHeight(100);
      else
        iConsole->SetCursorHeight(20);
    }
    break;

    case EMenuFontDialog:
    {
      TCharFormat charFormat;
      charFormat.iFontSpec = iConsole->Font();
      TCharFormatMask dummy;
      CEikFontDialog* dialog=new(ELeave) CEikFontDialog(charFormat,dummy);
      if (dialog->ExecuteLD(R_EIK_DIALOG_FONT))
      {
        charFormat.iFontSpec.iTypeface.SetIsProportional(EFalse);
        iConsole->SetFontL(charFormat.iFontSpec);
      }
    }
    break;

    case EMenuCommandHideCursor:
      iHideCursor=!iHideCursor;
      if (iHideCursor)
        iConsole->HideCursor();
      else
        iConsole->DrawCursor();
      break;
      case EMenuCommandIgnoreCursor:
      iConsole->SetKeepCursorInSight(iIgnoreCursor);
      iIgnoreCursor=!iIgnoreCursor;
      break;
      case EMenuCommandScrollLock:
      iScrollLock=!iScrollLock;
      iConsole->SetScrollLock(iScrollLock);
      break;
      case EMenuCommandPrintable:
      iAllPrintable=!iAllPrintable;
      iConsole->SetAllPrintable(iAllPrintable);
      break;

      case EMenuScrollNone:
      iConsole->SetScrollBarVisibilityL(CEikScrollBarFrame::EOff,CEikScrollBarFrame::EOff);
          break;
      case EMenuScrollHor:
      iConsole->SetScrollBarVisibilityL(CEikScrollBarFrame::EAuto,CEikScrollBarFrame::EOff);
          break;
      case EMenuScrollVert:
      iConsole->SetScrollBarVisibilityL(CEikScrollBarFrame::EOff,CEikScrollBarFrame::EAuto);
          break;
       case EMenuScrollBoth:
      iConsole->SetScrollBarVisibilityL(CEikScrollBarFrame::EAuto,CEikScrollBarFrame::EAuto);
          break;

    case EMenuCommandLongLine      :
      TBuf<256> str;
      for (TInt i=0; i<9; i++)
        {
        TBuf<32> tmp;
        tmp.Format(_L("%d abcdefghijklmnopqrstuvwxyz"),i);
        str+=tmp;
        }
      iConsole->Write(str);
      iConsole->FlushChars();
          break;
  }
}

void CConsoleControl::FocusChanged(TDrawNow aDrawNow)
{
  iConsole->ConsoleControl()->SetFocus(IsFocused(), aDrawNow);
}

void CConsoleControl::ToggleFontStyleAndRedrawL(TMessageControlFontStyle aStyleElement)
{
  switch (aStyleElement)
  {
    case EStyleElementColor:
      if ( iConsole->Att() & ATT_COLORMASK )  // color?
        iConsole->SetAtt(ATT_NORMAL);  // then set normal
      else                // else
        iConsole->SetAtt(4,11);      // set 4 (darkgray) on 11 (lightgray)
      break;
    case EStyleElementBold:
      // clear color flag (just to be sure) and switch bold flag
      iConsole->SetAtt( (iConsole->Att()&(~ATT_COLORMASK)) ^ ATT_BOLD );
      break;
    case EStyleElementItalic:
      // clear color flag (just to be sure) and switch italic flag
      iConsole->SetAtt( (iConsole->Att()&(~ATT_COLORMASK)) ^ ATT_ITALIC );
      break;
    case EStyleElementInverse:
      // clear color flag (just to be sure) and switch inverse flag
      iConsole->SetAtt( (iConsole->Att()&(~ATT_COLORMASK)) ^ ATT_INVERSE );
      break;
    case EStyleElementUnderline:
      // clear color flag (just to be sure) and switch underline flag
      iConsole->SetAtt( (iConsole->Att()&(~ATT_COLORMASK)) ^ ATT_UNDERLINE );
      break;
  }
}

void CConsoleControl::HandlePointerEventL(const TPointerEvent& aPointerEvent)
{
  TBuf<128> iMessage;
  iEikonEnv->Format128(iMessage,R_CONS_POINTER_EVENT,aPointerEvent.iType,aPointerEvent.iPosition.iX,aPointerEvent.iPosition.iY);
  iConsole->Write(iMessage);
  iConsole->FlushChars();
}

TKeyResponse CConsoleControl::OfferKeyEventL(const TKeyEvent& aKeyEvent,TEventCode aType)
{
  if (aType!=EEventKey)
    return(EKeyWasConsumed);
  TInt code=aKeyEvent.iCode;
//  TInt modifiers=aKeyEvent.iModifiers;
  if (code==CTRL('e'))
    CBaActiveScheduler::Exit();

//  TRect range = iConsole->Selection(); // get current selected range
  switch (code)
  {
    case EKeyUpArrow:
      if (history>0)
      {
        history--;
        HBufC* pL=(*iHistory)[history];
        Buf()=(*pL);
      }
      iCursorPos = iLine.Length();
      iConsole->SetPos(0);
      iConsole->ClearToEndOfLine();
      break;
    case EKeyDownArrow:
      if (history<iHistory->Count())
      {
        history++;
        if (history == iHistory->Count())
        {
          iLine.Zero();
          iLine.ZeroTerminate();
        }
        else
        {
          HBufC* pL=(*iHistory)[history];
          iLine = (*pL);
        }
      }
      iCursorPos = iLine.Length();
      iConsole->SetPos(0);
      iConsole->ClearToEndOfLine();

      break;
    case EKeyLeftArrow:
//      if (modifiers & EModifierShift)
//        {
//        range.iTl = iConsole->CursorPos();
//        iConsole->SetSelection(range);
//        }
//      else
      {
        if (iCursorPos>0)
          iCursorPos--;
      }

      break;
    case EKeyRightArrow:
//      iConsole->Right();
//      if (modifiers & EModifierShift)
//        {
//        range.iTl = iConsole->CursorPos();
//        iConsole->SetSelection(range);
//        }
//      else
      {
      if (iCursorPos<iLine.Length())
        iCursorPos++;
      break;
      }
    case EKeyEnter:
    {
      if (iLine.Length()>0)
      {
        iConsole->Cr();
        iConsole->Lf();
        iLine.ZeroTerminate();

        HBufC* pB=Buf().Alloc();
        if (pB!=NULL)
          {TRAPD(r,iHistory->AppendL(pB));}

        history = iHistory->Count();

        iLine.Append(';');
        iLine.ZeroTerminate();

        TBuf<KMaxYacasStringLength> input;
        TBuf<KMaxYacasStringLength> result;
        TInt ret;

        input = iLine.Ptr();
        ret = iYacasSession.SetFromString(input,result);
        iConsole->Printf(_L("Out> "));
        iConsole->Printf(result);
        iConsole->Printf(_L("\r\n"));

        iLine.Zero();
        iLine.ZeroTerminate();
        DrawInPrompt();
      }
    }
    break;

    case EKeyBackspace:
      if (iCursorPos>0)
      {
        iCursorPos--;
        iLine.Delete(iCursorPos,1);
      }
      break;
    case EKeyDelete:
      iLine.Delete(iCursorPos,1);
      break;
    case EKeyTab:
      {
        TInt prevhistory=history;
        history = iHistory->Count()-1;
        TBuf<256> line;
        line.Append(iLine);
        line.Append(_L("*")); // pattern match wildcard
        while (history>=0)
        {
          HBufC* pL=(*iHistory)[history];
          TInt match = pL->Match(line);
          if (match == KErrNotFound) // not found a match
            goto CONTINUE;

          // found a match
          iLine = (*pL);
          iCursorPos = iLine.Length();
          iConsole->SetPos(0);
          iConsole->ClearToEndOfLine();
          break;
        CONTINUE:
          history--;
        }
        if (history<0)
          history = prevhistory;
      }
      break;

    default:
    {
      iConsole->SelectCursor();  // forget previous selection
      if (iCursorPos == iLine.Length())
        iLine.Append(code);
      else
      {
        TBuf<1> c;
        c.Append(code);
        iLine.Insert(iCursorPos,c);
      }
      iCursorPos++;
      iLine.ZeroTerminate();
    }
    break;
  }

  iConsole->SetPos(0);
  iConsole->ClearToEndOfLine();
  iConsole->FlushChars();
  iConsole->SetPos(0);
  iLine.ZeroTerminate();
  iConsole->Printf(_L("In> %s "),iLine.Ptr());
  iConsole->SetPos(iCursorPos+4);
  iConsole->FlushChars();

  return(EKeyWasConsumed);
}

//
// CSimpleAppUi
//

void CSimpleAppUi::ConstructL()
{
  BaseConstructL();
  CreateConsoleL(CEikConsoleScreen::ENoInitialCursor);
}

void CSimpleAppUi::CreateConsoleL(TInt aFlags)
{
  iConsole=new(ELeave) CConsoleControl(((CSimpleDocument*)iDocument)->Application()->AppFullName());
  iConsole->ConstructL(aFlags);
//  iConsole->ConstructL(TPoint(4,4),TSize(60,16),aFlags);
  AddToStackL(iConsole);
  iConsole->ActivateL();
}

CSimpleAppUi::~CSimpleAppUi()
{
  delete(iConsole);
}

void CSimpleAppUi::DynInitMenuPaneL(TInt aMenuId,CEikMenuPane* aMenuPane)
{
  if (aMenuId==R_CONS_SPECIAL_MENU)
  {
    if (iBackedUp)
      aMenuPane->SetItemButtonState(EMenuWindowType,EEikMenuItemSymbolOn);
  }
  iConsole->DynInitMenuPaneL(aMenuId, aMenuPane);
}

void CSimpleAppUi::HandleCommandL(TInt aCommand)
{
  switch (aCommand)
  {
    case EMenuWindowType:
    {
      iBackedUp = !iBackedUp;
      RemoveFromStack(iConsole);
      delete iConsole;
      TInt flags=0;
      if (iBackedUp)
        flags|=CEikConsoleScreen::EUseBackedUpWindow;
      CreateConsoleL(flags);
    }
    break;
    default:
      iConsole->HandleCommandL(aCommand);
  }
}

//
// CSimpleDocument
//


CEikAppUi* CSimpleDocument::CreateAppUiL()
{
  return(new(ELeave) CSimpleAppUi);
}

//
// CSimpleApplication
//

class CSimpleApplication : public CEikApplication
{
private: // from CApaApplication
  CApaDocument* CreateDocumentL();
  TUid AppDllUid() const;
};

const TUid KUidSimpleApp={227};

TUid CSimpleApplication::AppDllUid() const
{
  return(KUidYacasApp);
}

CApaDocument* CSimpleApplication::CreateDocumentL()
{
  return(new(ELeave) CSimpleDocument(*this));
}

//
// EXPORTed functions
//

EXPORT_C CApaApplication* NewApplication()
{
  return(new CSimpleApplication);
}

GLDEF_C TInt E32Dll(TDllReason)
{
  return(KErrNone);
}

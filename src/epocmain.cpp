
// - PopLocalFrame in the LispEnvironment destructor!!!
// - DestructArray not always working in the grower.inl deleting array grower. so copy-paste

#include <basched.h>
#include <eikenv.h>
#include <coecntrl.h>
#include <eikappui.h>
#include <e32keys.h>
#include <eikmenup.h>
#include <eikmenu.hrh>
#include <eikdef.h>
#include <eikconso.h>
#include <eikfontd.h>
#include <eikapp.h>
#include <eikdoc.h>
#include <eikon.rsg>

#include "epocyacas.hrh"
#include <epocyacas.rsg>

#include "yacas.h"
#include "commandline.h"
#include "standard.h"

#define KHistoryFile   _L("yacashistory")


#define HAS_YACAS


enum TMessageControlFontStyle
    {
    EStyleElementBold=EMenuCommandBold,
    EStyleElementItalic=EMenuCommandItalic,
    EStyleElementInverse=EMenuCommandInverse,
    EStyleElementUnderline=EMenuCommandUnderline,
    EStyleElementColor=EMenuCommandColor
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


class CConsoleControl : public CCoeControl
	{
public:
  CConsoleControl(const TDes& aAppFullName) : iAppFullName(aAppFullName){}
	~CConsoleControl();
	void ConstructL(TInt aFlags);
	void ConstructL(TPoint aLeftTop,const TSize& aSize,TInt aFlags);
  void ConstructYacas();
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

protected:
    void FocusChanged(TDrawNow aDrawNow);
private:
    void ToggleFontStyleAndRedrawL(TMessageControlFontStyle aStyleElement);
private:
	CEikConsoleScreen* iConsole;
  TInt iAllPrintable,iScrollLock,iIgnoreCursor,iHideCursor;
	TDesC* iSelBufPtr;
	TInt iSmallScreen;
	TInt iHighCursor;

//#ifdef HAS_YACAS
  CYacas* iYacas;
//#endif

  TFileName iAppFullName;

  TBuf<256> iLine;
  TInt iCursorPos;

  CDeletingArrayGrower<LispStringPtr> iHistory;
  TInt history;
  LispString stringOutput;
  StringOutput *theOutput; 
	};




void CConsoleControl::LoadHistory()
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
void CConsoleControl::SaveHistory()
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


void CConsoleControl::ConstructYacas()
{

  TFileName name;

  
  {
    TFileName base;
    base.Format(_L("yacasinit"));

    TParse parser;
    parser.Set(base,&iAppFullName,NULL);
    parser.AddDir(_L("scripts"));
    name = parser.DriveAndPath();
    name.ZeroTerminate();
  }    

  theOutput = new StringOutput(stringOutput);
  iYacas = CYacas::NewL(theOutput);
#ifdef HAS_YACAS


    TBuf<256> buf;
    buf.Format(_L("DefaultDirectory(\"%s\");"),name.Ptr());

    TBuf<256> buf2;
    buf2.Zero();
    {
      TInt i;
      for (i=0;i<buf.Length();i++)
      {
        if (buf[i] == '\\')
        {
          buf2.Append(buf[i]);
        }
        buf2.Append(buf[i]);
      }
    }
    
    
    buf2.ZeroTerminate();
   
    iYacas->Evaluate((char*)buf2.Ptr());
    iYacas->Evaluate("Load(\"yacasinit\");");
    ShowResult("");
#endif


}

void CConsoleControl::ShowResult(char *prompt)
{
#ifdef HAS_YACAS
  if (iYacas->Error()[0] != '\0')
  {
    iConsole->Printf(_L("%s\r\n"),iYacas->Error());
  }
  else
  {
    if (theOutput->iString.String()[0])
    {
      iConsole->Write(_L(&theOutput->iString.String()[0]));
      theOutput->iString.SetNrItems(0);
      theOutput->iString.Append('\0');
    }
    iConsole->Write(_L(prompt));
    iConsole->Write(_L(iYacas->Result()));
    iConsole->Write(_L("\r\n"));
  }
#endif
}

CConsoleControl::~CConsoleControl()
	{
  SaveHistory();

//#ifdef HAS_YACAS
  delete iYacas;
//#endif
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
  LoadHistory();

  iConsole=new(ELeave) CEikConsoleScreen;

    
    iConsole->ConstructL(_L("Yacas"),aFlags);
	iConsole->SetHistorySizeL(10,10);
  iLine.Zero();
  ConstructYacas();
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
    ConstructYacas();
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
  DrawInPrompt();
	iConsole->FlushChars();
	iConsole->DrawCursor();
	iConsole->SetAtt(ATT_NORMAL);
	iConsole->DrawCursor();
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

	case EMenuCommandEditCopy			:
		{
		TRect range = iConsole->Selection();	// get current selected range		
		if (iSelBufPtr) delete iSelBufPtr;		// forget previous selection
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
	case EMenuCommandEditPaste			:
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
//        iConsole->ConsoleControl()->SetExtentL( TPoint(0,0), TSize(640,240) );
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

	case EMenuCommandLongLine			:
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
		if ( iConsole->Att() & ATT_COLORMASK )	// color?
			iConsole->SetAtt(ATT_NORMAL);	// then set normal
		else								// else
			iConsole->SetAtt(4,11);			// set 4 (darkgray) on 11 (lightgray)
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
//    TInt modifiers=aKeyEvent.iModifiers;
    TInt code=aKeyEvent.iCode;
    if (code==CTRL('e'))
        CBaActiveScheduler::Exit();


	TRect range = iConsole->Selection(); // get current selected range
	switch (code)
		{
		case EKeyUpArrow:
      if (history>0)
      {
        history--;
        iLine = _L(&((*iHistory[history])[0]));
      }
      iCursorPos = iLine.Length();
      iConsole->SetPos(0);
      iConsole->ClearToEndOfLine();
      break;
		case EKeyDownArrow:
      if (history<iHistory.NrItems())
      {
        history++;
        if (history == iHistory.NrItems())
        {
          iLine.Zero();
          iLine.ZeroTerminate();
        }
        else
        {
          iLine = _L(&((*iHistory[history])[0]));
        }
      }
      iCursorPos = iLine.Length();
      iConsole->SetPos(0);
      iConsole->ClearToEndOfLine();

      break;
		case EKeyLeftArrow:
//			iConsole->Left();
      if (iCursorPos>0)
        iCursorPos--;

      break;
		case EKeyRightArrow:
      if (iCursorPos<iLine.Length())
        iCursorPos++;
      break;
		case EKeyEnter: 
				{
          if (iLine.Length()>0)
          {
            iConsole->Cr();
				    iConsole->Lf();
            iLine.ZeroTerminate();
            LispStringPtr ptr = new LispString;
            *ptr = (LispCharPtr)iLine.Ptr();
            iHistory.Append(ptr);
            history = iHistory.NrItems();

            iLine.Append(';');
            iLine.ZeroTerminate();
          
#ifdef HAS_YACAS
            iYacas->Evaluate((char*)iLine.Ptr());
            ShowResult("Out> ");
#endif
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
        LispInt prevhistory=history;
        history = iHistory.NrItems()-1;
        while (history>=0)
        {
            LispInt j=0;
            while (j<iLine.Length()-1 &&
                   j<iHistory[history]->NrItems())
            {
                if (iLine[j] != (*iHistory[history])[j])
                    goto CONTINUE;
                j++;
            }

            iLine = _L(&((*iHistory[history])[0]));
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
        iConsole->SelectCursor();	// forget previous selection
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

void CSimpleAppUi::ConstructL()
    {
    BaseConstructL();
	CreateConsoleL(CEikConsoleScreen::ENoInitialCursor);
    }

void CSimpleAppUi::CreateConsoleL(TInt aFlags)
	{
/*
	TBuf<30> msg;
	if (iBackedUp)
		msg=_L("Using a backed up window");
	else
		msg=_L("Using a normal window");
	iEikonEnv->InfoMsg(msg);
*/
  iConsole=new(ELeave) CConsoleControl(((CSimpleDocument*)iDocument)->Application()->AppFullName());
	iConsole->ConstructL(aFlags);
//	iConsole->ConstructL(TPoint(4,4),TSize(60,16),aFlags);
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
	return(KUidSimpleApp);
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

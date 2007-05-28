
#include "yacasprivate.h"
#include "commandline.h"

//TODO maybe fix the assignment operator on String?
static inline void CopyString(LispString& aTrg, LispString& aSrc)
{
  aTrg.ResizeTo(0);
  LispInt i;
  for (i=0;i<aSrc.Size();i++)
  {
    aTrg.Append(aSrc[i]);
  }
//  aTrg.Append('\0');
}


CCommandLine::~CCommandLine()
{
}


void CCommandLine::GetHistory(LispInt aLine)
{
    iSubLine.ResizeTo(0);
    LispInt i;
    LispString * line = iHistoryList.GetLine(aLine);
    for (i=0;i<line->Size();i++)
    {
        iSubLine.Append((*line)[i]);
    }
}

void CCommandLine::MaxHistoryLinesSaved(LispInt aNrLines)
{
}

void CCommandLine::ReadLine(LispChar * prompt)
{
    iLine.ResizeTo(0);

NEXTLINE:
    iSubLine.ResizeTo(1);
    iSubLine[0] = '\0';
    ReadLineSub(prompt);

    {
        LispInt i,nr;
        nr = iSubLine.Size()-1;
        for(i=0;i<nr;i++)
            iLine.Append(iSubLine[i]);
        if (nr>0)
            if (iSubLine[nr-1] == '\\')
            {
                iLine.ResizeTo(iLine.Size()-1);
                goto NEXTLINE;
            }
    }
    iLine.Append('\0');
}

void CCommandLine::ReadLineSub(LispChar * prompt)
{
    LispInt cursor=0;
    int promptlen = PlatStrLen(prompt);
 
    iHistoryList.ResetHistoryPosition();
    iHistoryUnchanged = 0;

    iFullLineDirty = 1;
    ShowLine(prompt,promptlen,cursor);

    for (;;)
    {
        int c;
        c=GetKey();
        switch (c)
        {
        case eDelete:
            if (cursor<iSubLine.Size()-1)
            {
                iSubLine.Delete(cursor);
                iFullLineDirty = 1;
                iHistoryUnchanged = 0;
            }
            break;
        case eBackSpace:
            if (cursor>0)
            {
                cursor--;
                iSubLine.Delete(cursor);
                iFullLineDirty = 1;
                iHistoryUnchanged = 0;
            }
            break;
        case eLeft:
            if (cursor>0)
                cursor--;
            break;
        case eRight:
            if (cursor<iSubLine.Size()-1)
                cursor++;
            break;


      case eUp:
        if (iHistoryList.ArrowUp(iSubLine,cursor))
        {
          iFullLineDirty = 1;
          iHistoryUnchanged = 1;
        }
        break;
      case eDown:

        if (iHistoryList.ArrowDown(iSubLine,cursor))
          {
            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
          }
          else
          {
            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
          }
        break;


        case eTab:
            iHistoryList.Complete(iSubLine,cursor);
            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
            break;
        case eEscape:
            iSubLine.ResizeTo(1);
            iSubLine[0] = '\0';
            cursor = iSubLine.Size()-1;
            iFullLineDirty = 1;
            iHistoryUnchanged = 0;
            iHistoryList.ResetHistoryPosition();
            break;
        case eHome:
            cursor=0;
            break;
        case eEnd:
            cursor=iSubLine.Size()-1;
            break;
        case eEnter:
            if (iSubLine.Size()>1)
            {
              NewLine();
              iHistoryList.AddLine(iSubLine);
              return;
            }
            iFullLineDirty = 1;
            break;
        default:
            {
                LispChar cc=(LispChar)c;
                iSubLine.Insert(cursor,cc);
                iFullLineDirty = 1;
                iHistoryUnchanged = 0;
            }
            cursor++;
            break;
        }
        switch (c)
        {
        case ')': ShowOpen(prompt,promptlen,'(',')',cursor); break;
        case '}': ShowOpen(prompt,promptlen,'{','}',cursor); break;
        case ']': ShowOpen(prompt,promptlen,'[',']',cursor); break;
        case '\"': ShowOpen(prompt,promptlen,'\"','\"',cursor); break;
        }
        ShowLine(prompt,promptlen,cursor);
    }
}

void CCommandLine::ShowOpen(LispChar * prompt,LispInt promptlen,
                            LispChar aOpen, LispChar aClose, LispInt aCurPos)
{
    LispInt count=1;
    aCurPos--;
    aCurPos--;
    while (aCurPos>0 && count>0)
    {
        if (iSubLine[aCurPos] == aOpen)
        {
            count--;
        }
        else if (iSubLine[aCurPos] == aClose)
        {
            count++;
        }
        aCurPos--;
    }
    if (count == 0)
    {
        ShowLine(prompt,promptlen,aCurPos+1);
        Pause();
    }
}

void CConsoleHistory::Append(LispString * aString)
{
  iHistory.Append(aString);
  history=iHistory.Size();
}

void CConsoleHistory::AddLine(LispString& aString)
{


  LispInt historyChanged = 0;
  if (!(history<iHistory.Size()))
  {
    historyChanged=1;
    history++;
  }
  else if (!(*iHistory[history] == aString))
  {
    historyChanged = 1;
  }
 

  if (historyChanged)
  {
    LispString * ptr = NEW LispString();
    CopyString(*ptr, aString);
    iHistory.Append(ptr);
    return;
  }
  else
  {
    LispString * orig = iHistory[history];
    LispInt i;
    for (i=history;i<iHistory.Size()-1;i++)
    {
      iHistory[i] = iHistory[i+1];
    }
    iHistory[iHistory.Size()-1] = orig;
    return;
  }
}


LispInt CConsoleHistory::ArrowUp(LispString& aString,LispInt &aCursorPos)
{
  LispString prefix;
  //if (aCursorPos == aString.Size()-1) aCursorPos = 0;
  prefix.SetStringCounted(aString.c_str(),aCursorPos);

  int i = history - 1;

//printf("Searching for [%s] starting at %d (of %d)\n",prefix.c_str(),i,iHistory.Size());
  LispString histpre;
  while (i >= 0)
  {
    histpre.SetStringCounted(iHistory[i]->c_str(),aCursorPos);
    if (histpre == prefix)
      break;
    i--;
  }
  if (i >= 0 && i != history && histpre == prefix)
  {
    history = i;
    CopyString(aString, (*iHistory[history]));
//    if (aCursorPos == 0) aCursorPos = aString.Size()-1;
    return 1;
  }
  return 0;
}

LispInt CConsoleHistory::ArrowDown(LispString& aString,LispInt &aCursorPos)
{
  LispString prefix;
  //  if (aCursorPos == aString.Size()-1) aCursorPos = 0;
  prefix.SetStringCounted(aString.c_str(),aCursorPos);
  int i = history + 1;
  LispString histpre;
  while (i < iHistory.Size())
  {
    histpre.SetStringCounted(iHistory[i]->c_str(),aCursorPos);
    if (histpre == prefix)
      break;
    i++;
  }
  if (i < iHistory.Size() && histpre == prefix)
  {
    history = i;
    CopyString(aString, (*iHistory[history]));
    return 1;
  }
  else
  {
    history = iHistory.Size();
    CopyString(aString,prefix);
  }
  return 0;
}

LispInt CConsoleHistory::NrLines()
{
  return iHistory.Size();
}

LispString * CConsoleHistory::GetLine(LispInt aLine)
{
  return iHistory[aLine];
}
void CConsoleHistory::ResetHistoryPosition()
{
  history=iHistory.Size();
}

LispInt CConsoleHistory::Complete(LispString& aString,LispInt &aCursorPos)
{
    LispInt prevhistory=history;
    history = iHistory.Size()-1;
    while (history>=0)
    {
        LispInt j=0;
        while (j<aString.Size()-1 &&
                j<iHistory[history]->Size())
        {
            if (aString[j] != (*iHistory[history])[j])
                goto CONTINUE;
            j++;
        }
        CopyString(aString, (*iHistory[history]));
        aCursorPos = aString.Size()-1;
        break;
    CONTINUE:
        history--;
    }
    if (history<0)
        history = prevhistory;

    return 1;
}



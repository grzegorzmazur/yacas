
#include "yacasprivate.h"
#include "commandline.h"


CCommandLine::~CCommandLine()
{
    /*TODO remove???
     LispInt i;
    for (i=0;i<iHistory.NrItems();i++)
    {
        delete iHistory[i];
        }
        */
}


void CCommandLine::GetHistory(LispInt aLine)
{
    iSubLine.SetNrItems(0);
    LispInt i;
    for (i=0;i<iHistory[aLine]->NrItems();i++)
    {
        iSubLine.Append((*iHistory[aLine])[i]);
    }
}

void CCommandLine::MaxHistoryLinesSaved(LispInt aNrLines)
{
}

void CCommandLine::ReadLine(LispCharPtr prompt)
{
    iLine.SetNrItems(0);

NEXTLINE:
    iSubLine.SetNrItems(1);
    iSubLine[0] = '\0';
    ReadLineSub(prompt);

    {
        LispInt i,nr;
        nr = iSubLine.NrItems()-1;
        for(i=0;i<nr;i++)
            iLine.Append(iSubLine[i]);
        if (nr>0)
            if (iSubLine[nr-1] == '\\')
            {
                iLine.SetNrItems(iLine.NrItems()-1);
                goto NEXTLINE;
            }
    }
    iLine.Append('\0');
}

void CCommandLine::ReadLineSub(LispCharPtr prompt)
{
    LispInt cursor=0;
    int promptlen = PlatStrLen(prompt);

    if (//iHistoryUnchanged &&
        history<iHistory.NrItems()-1 && iTraceHistory)
    {
        history++;
        GetHistory(history);
        cursor = iSubLine.NrItems()-1;
    }
    else
    {
        history=iHistory.NrItems();
        iHistoryUnchanged = 0;
    }
    
    
    iFullLineDirty = 1;
    ShowLine(prompt,promptlen,cursor);

    for (;;)
    {
        int c;
        c=GetKey();
        switch (c)
        {
        case eDelete:
            if (cursor<iSubLine.NrItems()-1)
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
            if (cursor<iSubLine.NrItems()-1)
                cursor++;
            break;






      case eUp:
        {
          LispString prefix;
          prefix.SetStringCounted(iSubLine.String(),cursor);

          int i = history - 1;

//printf("Searching for [%s] starting at %d (of %d)\n",prefix.String(),i,iHistory.NrItems());
          LispString histpre;
          while (i >= 0)
          {
            histpre.SetStringCounted(iHistory[i]->String(),cursor);
            if (histpre == prefix)
              break;
            i--;
          }
          if (i >= 0 && i != history && histpre == prefix)
          {
            history = i;
            GetHistory(history);

            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
          }
        }
        break;
      case eDown:
        {
          LispString prefix;
          prefix.SetStringCounted(iSubLine.String(),cursor);
          int i = history + 1;
          LispString histpre;
          while (i < iHistory.NrItems())
          {
            histpre.SetStringCounted(iHistory[i]->String(),cursor);
            if (histpre == prefix)
              break;
            i++;
          }
          if (i < iHistory.NrItems() && histpre == prefix)
          {
            history = i;
            GetHistory(history);
            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
          }
          else 
          {

//printf("end encountered: %d [%s]\n",i,prefix.String());
            history = iHistory.NrItems();
//            iSubLine = prefix;

            {
              iSubLine.SetNrItems(0);
              LispInt i;
              for (i=0;i<prefix.NrItems();i++)
              {
                iSubLine.Append(prefix[i]);
              }
              iSubLine.Append('\0');
            }
//printf("set to: %d [%s]\n",history,iSubLine.String());

//            int pos = cursorPos;
//            ResetInput();
//            inputLine = prefix;
//            cursorPos = pos;
            iFullLineDirty = 1;
            iHistoryUnchanged = 1;
          }
        }
        break;



/*

        case eUp:
            if (history>0)
            {
                history--;
                GetHistory(history);
                cursor = iSubLine.NrItems()-1;
                iFullLineDirty = 1;
                iHistoryUnchanged = 1;
            }
            break;
        case eDown:
            if (history<iHistory.NrItems()-1)
            {
                history++;
                GetHistory(history);
                cursor = iSubLine.NrItems()-1;
                iFullLineDirty = 1;
                iHistoryUnchanged = 1;
            }
            else if (history == iHistory.NrItems()-1)
            {
                iSubLine.SetNrItems(1);
                iSubLine[0] = '\0';
                cursor = iSubLine.NrItems()-1;
                history++;
                iFullLineDirty = 1;
            }
            break;
*/
        case eTab:
            {
                LispInt prevhistory=history;
                history = iHistory.NrItems()-1;
                while (history>=0)
                {
                    LispInt j=0;
                    while (j<iSubLine.NrItems()-1 &&
                           j<iHistory[history]->NrItems())
                    {
                        if (iSubLine[j] != (*iHistory[history])[j])
                            goto CONTINUE;
                        j++;
                    }

                    GetHistory(history);
                    cursor = iSubLine.NrItems()-1;
                    iFullLineDirty = 1;
                    iHistoryUnchanged = 1;
                    break;
                CONTINUE:
                    history--;
                }
                if (history<0)
                    history = prevhistory;
            }
            break;
        case eEscape:
            iSubLine.SetNrItems(1);
            iSubLine[0] = '\0';
            cursor = iSubLine.NrItems()-1;
            iFullLineDirty = 1;
            iHistoryUnchanged = 0;
            history=iHistory.NrItems();
            break;
        case eHome:
            cursor=0;
            break;
        case eEnd:
            cursor=iSubLine.NrItems()-1;
            break;
        case eEnter:
            if (iSubLine.NrItems()>1)
            {
                NewLine();
                LispStringPtr ptr = NEW LispString();
                *ptr = iSubLine.String();
                if (!iHistoryUnchanged)
                  iHistory.Append(ptr);
            }
            return;
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

void CCommandLine::ShowOpen(LispCharPtr prompt,LispInt promptlen,
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

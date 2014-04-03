#include "yacas/yacasprivate.h"
#include "yacas/commandline.h"


void CCommandLine::GetHistory(LispInt aLine)
{
    iSubLine = iHistoryList.GetLine(aLine).c_str();
}

void CCommandLine::MaxHistoryLinesSaved(LispInt aNrLines)
{
}

void CCommandLine::ReadLine(const std::string& prompt)
{
    iLine.clear();

    bool next_line;

    do {
        next_line = false;
        iSubLine.clear();

        ReadLineSub(prompt);

        iLine.append(iSubLine);
        const std::size_t n = iLine.size();
        if (n && iLine[n - 1] == '\\') {
            iLine.resize(n - 1);
            next_line = true;
        }
    } while (next_line);
}

void CCommandLine::ReadLineSub(const std::string& prompt)
{
    unsigned cursor = 0;
 
    iHistoryList.ResetHistoryPosition();
    history_unchanged = false;

    full_line_dirty = true;
    ShowLine(prompt, cursor);

    for (;;)
    {
        const int c = GetKey();

        switch (c)
        {
        case eDelete:
            if (cursor < iSubLine.size())
            {
                iSubLine.erase(cursor, 1);
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        case eBackSpace:
            if (cursor>0)
            {
                iSubLine.erase(--cursor, 1);
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        case eLeft:
            if (cursor>0)
                cursor--;
            break;
        case eRight:
            if (cursor<iSubLine.size())
                cursor++;
            break;


      case eUp:
        if (iHistoryList.ArrowUp(iSubLine,cursor))
        {
          full_line_dirty = true;
          history_unchanged = true;
        }
        break;
      case eDown:

        if (iHistoryList.ArrowDown(iSubLine,cursor))
          {
            full_line_dirty = true;
            history_unchanged = true;
          }
          else
          {
            full_line_dirty = true;
            history_unchanged = true;
          }
        break;


        case eTab:
            iHistoryList.Complete(iSubLine, cursor);
            full_line_dirty = true;
            history_unchanged = true;
            break;
        case eEscape:
            iSubLine = "";
            cursor = 0;
            full_line_dirty = true;
            history_unchanged = false;
            iHistoryList.ResetHistoryPosition();
            break;
        case eHome:
            cursor=0;
            break;
        case eEnd:
            cursor=iSubLine.size();
            break;
        case eEnter:
            if (!iSubLine.empty())
            {
              NewLine();
              iHistoryList.AddLine(iSubLine);
              return;
            }
            full_line_dirty = true;
            break;
        case eKill:
            if (cursor < iSubLine.size())
            {
                iSubLine.erase(cursor);
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        default:
            {
                LispChar cc=(LispChar)c;
                iSubLine.insert(cursor, 1, cc);
                full_line_dirty = true;
                history_unchanged = false;
            }
            cursor++;
            break;
        }
        switch (c)
        {
        case ')': ShowOpen(prompt, '(', ')', cursor); break;
        case '}': ShowOpen(prompt, '{', '}', cursor); break;
        case ']': ShowOpen(prompt, '[', ']', cursor); break;
        case '\"': ShowOpen(prompt, '\"', '\"', cursor); break;
        }
        ShowLine(prompt, cursor);
    }
}

void CCommandLine::ShowOpen(const std::string& prompt,
                            LispChar aOpen, LispChar aClose,
                            LispInt aCurPos)
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
        ShowLine(prompt, aCurPos+1);
        Pause();
    }
}

void CConsoleHistory::Append(const std::string& s)
{
    iHistory.push_back(s);
    history = iHistory.size();
}

void CConsoleHistory::AddLine(const std::string& aString)
{
    bool history_changed = false;

    if (history >= iHistory.size()) {
        history_changed = true;
        history++;
    } else if (iHistory[history] != aString) {
        history_changed = true;
    }
 

    if (history_changed) {
        iHistory.push_back(aString);
        return;
    }

    const std::string orig = iHistory[history];
    iHistory.erase(iHistory.begin() + history);
    iHistory.push_back(orig);
}


bool CConsoleHistory::ArrowUp(std::string& aString, unsigned& aCursorPos)
{
  const std::string prefix(aString.c_str(), aCursorPos);
  //if (aCursorPos == aString.Size()-1) aCursorPos = 0;

  if (!history)
      return false;

  std::size_t i = history - 1;

//printf("Searching for [%s] starting at %d (of %d)\n",prefix.c_str(),i,iHistory.Size());
  std::string histpre;
  while (i >= 0) {
    histpre = std::string(iHistory[i].c_str(), aCursorPos);
    if (histpre == prefix)
      break;
    i--;
  }

  if (i >= 0 && i != history && histpre == prefix)
  {
    history = i;
    aString = iHistory[history];
//    if (aCursorPos == 0) aCursorPos = aString.Size()-1;
    return true;
  }
  return false;
}

bool CConsoleHistory::ArrowDown(std::string& aString, unsigned& aCursorPos)
{
  const std::string prefix(aString.c_str(), aCursorPos);
  //  if (aCursorPos == aString.Size()-1) aCursorPos = 0;

  std::size_t i = history + 1;
  std::string histpre;
  while (i < iHistory.size())
  {
    histpre = std::string(iHistory[i].c_str(), aCursorPos);
    if (histpre == prefix)
      break;
    i++;
  }
  if (i < iHistory.size() && histpre == prefix)
  {
    history = i;
    aString = iHistory[history];
    return true;
  }
  else
  {
    history = iHistory.size();
    aString = prefix;
  }
  return false;
}

LispInt CConsoleHistory::NrLines()
{
  return iHistory.size();
}

const std::string& CConsoleHistory::GetLine(LispInt aLine)
{
  return iHistory[aLine];
}
void CConsoleHistory::ResetHistoryPosition()
{
  history = iHistory.size();
}

bool CConsoleHistory::Complete(std::string& aString, unsigned& aCursorPos)
{
    if (!history)
        return false;

    std::size_t prevhistory = history;
    history = iHistory.size() - 1;
    while (history>=0)
    {
        std::size_t j = 0;
        while (j < aString.size() &&
               j < iHistory[history].size())
        {
            if (aString[j] != iHistory[history][j])
                goto CONTINUE;
            j++;
        }
        aString = iHistory[history];
        aCursorPos = aString.size();
        break;
    CONTINUE:
        history--;
    }
    if (history < 0)
        history = prevhistory;

    return true;
}



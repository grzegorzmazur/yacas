#include "yacas/yacasprivate.h"
#include "yacas/commandline.h"

#include <algorithm>

namespace {
    struct IsPrefix {
        
        IsPrefix(const std::string& p): _p(p), _l(p.length()) {}
        
        bool operator() (const std::string& s) const {
            return _p.compare(0, _l, s, 0, _l) == 0;
        }
        
    private:
        std::string _p;
        std::size_t _l;
    };
}

void CCommandLine::GetHistory(std::size_t aLine)
{
    iSubLine = iHistoryList.GetLine(aLine).c_str();
}

void CCommandLine::MaxHistoryLinesSaved(std::size_t aNrLines)
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
            history_unchanged = true;
            full_line_dirty = true;
            iHistoryList.ArrowUp(iSubLine,cursor);
            break;
        case eDown:
            history_unchanged = true;
            full_line_dirty = true;
            iHistoryList.ArrowDown(iSubLine,cursor);
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
                LispChar cc = c;
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
                             unsigned aCurPos)
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

CConsoleHistory::CConsoleHistory():
    history(0)
{
}

void CConsoleHistory::Append(const std::string& s)
{
    iHistory.push_back(s);
    history = iHistory.size();
}

void CConsoleHistory::AddLine(const std::string& s)
{
    bool history_changed = false;

    if (history >= iHistory.size()) {
        history_changed = true;
        history++;
    } else if (iHistory[history] != s) {
        history_changed = true;
    }
 

    if (history_changed) {
        iHistory.push_back(s);
        return;
    }

    const std::string orig = iHistory[history];
    iHistory.erase(iHistory.begin() + history);
    iHistory.push_back(orig);
}


bool CConsoleHistory::ArrowUp(std::string& s, unsigned c)
{
    if (history == 0)
        return false;

    const std::string prefix(s.c_str(), c);

    std::vector<std::string>::reverse_iterator p = iHistory.rbegin();
    std::advance(p, iHistory.size() - history);
    
    const std::vector<std::string>::reverse_iterator q =
            std::find_if(p, iHistory.rend(), IsPrefix(prefix));
    
    if (q == iHistory.rend())
        return false;
    
    s = *q;
    history -= std::distance(p, q) + 1;
    return true;
}

bool CConsoleHistory::ArrowDown(std::string& s, unsigned c)
{
    if (history > iHistory.size())
        return false;
    
    const std::string prefix(s.c_str(), c);

    std::vector<std::string>::iterator p = iHistory.begin();
    std::advance(p, history + 1);

    const std::vector<std::string>::iterator q =
            std::find_if(p, iHistory.end(), IsPrefix(prefix));

    if (q != iHistory.end()) {
        s = *q;
        history += std::distance(p, q) + 1;
        return true;
    } else {
        history = iHistory.size();
        s = prefix;
        return false;
    }
}

std::size_t CConsoleHistory::NrLines()
{
  return iHistory.size();
}

const std::string& CConsoleHistory::GetLine(std::size_t n)
{
  return iHistory[n];
}
void CConsoleHistory::ResetHistoryPosition()
{
  history = iHistory.size();
}

bool CConsoleHistory::Complete(std::string& s, unsigned& c)
{
    if (history == 0)
        return false;

    const std::size_t old_history = history;
    
    history = iHistory.size() - 1;
    
    const std::string prefix(s.c_str(), c);

    std::vector<std::string>::reverse_iterator p = iHistory.rbegin();
    std::advance(p, iHistory.size() - history);
    
    const std::vector<std::string>::reverse_iterator q =
            std::find_if(p, iHistory.rend(), IsPrefix(prefix));
    
    if (q == iHistory.rend()) {
        history = old_history;
        return false;
    }
    
    s = *q;
    c = s.length();
    history -= std::distance(p, q) + 1;
    return true;
}
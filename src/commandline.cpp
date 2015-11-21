#include "yacas/yacasprivate.h"
#include "yacas/commandline.h"

#include <algorithm>
#include <cstdint>

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
    iSubLine = iHistoryList.GetLine(aLine);
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
       

#ifdef YACAS_UINT32_T_IN_GLOBAL_NAMESPACE
		uint32_t c = GetKey();
#else
		std::uint32_t c = GetKey();
#endif
        const std::size_t len = utf8::distance(iSubLine.begin(), iSubLine.end());
        
        switch (c)
        {
        case eDelete:
            if (cursor < len)
            {
                std::string::iterator i = iSubLine.begin();
                utf8::advance(i, cursor, iSubLine.end());
                std::string::iterator j = i;
                utf8::next(j, iSubLine.end());
                iSubLine.erase(i, j);
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        case eBackSpace:
            if (cursor>0)
            {
                cursor -= 1;
                std::string::iterator i = iSubLine.begin();
                utf8::advance(i, cursor, iSubLine.end());
                std::string::iterator j = i;
                utf8::next(j, iSubLine.end());
                iSubLine.erase(i, j);
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        case eLeft:
            if (cursor>0)
                cursor--;
            break;
        case eRight:
            if (cursor < len)
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
            cursor = 0;
            break;
        case eEnd:
            cursor = len;
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
            if (cursor < len)
            {
                std::string::iterator i = iSubLine.begin();
                utf8::advance(i, cursor, iSubLine.end());
                iSubLine.erase(i, iSubLine.end());
                full_line_dirty = true;
                history_unchanged = false;
            }
            break;
        default:
            {
                std::string octets;
                utf8::append(c, std::back_inserter(octets));
                std::string::iterator i = iSubLine.begin();
                utf8::advance(i, cursor, iSubLine.end());
                iSubLine.insert(i, octets.begin(), octets.end());
                full_line_dirty = true;
                history_unchanged = false;
                cursor++;
            }
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
                            unsigned cursor)
{
    if (cursor < 2)
        return;
    
    cursor -= 2;
    
    LispInt count = 1;

    std::string::iterator p = iSubLine.begin();
    utf8::advance(p, cursor, iSubLine.end());

    for (;;) {
        if (*p == aOpen)
            count--;
        else if (*p == aClose)
            count++;

        if (count == 0)
            break;
        
        if (p == iSubLine.begin())
            break;
        
        utf8::prior(p, iSubLine.begin());
        cursor -= 1;
    }

    if (count == 0) {
        ShowLine(prompt, cursor);
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

    std::string::iterator i = s.begin();
    utf8::advance(i, c, s.end());
    
    const std::string prefix(s.begin(), i);

    auto p = iHistory.rbegin();
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

    std::string::iterator i = s.begin();
    utf8::advance(i, c, s.end());
    
    const std::string prefix(s.begin(), i);

    auto p = iHistory.begin();
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
    
    std::string::iterator i = s.begin();
    utf8::advance(i, c, s.end());
    const std::string prefix(s.begin(), i);

    auto p = iHistory.rbegin();
    std::advance(p, iHistory.size() - history);
    
    const std::vector<std::string>::reverse_iterator q =
            std::find_if(p, iHistory.rend(), IsPrefix(prefix));
    
    if (q == iHistory.rend()) {
        history = old_history;
        return false;
    }
    
    s = *q;
    c = utf8::distance(s.begin(), s.end());
    history -= std::distance(p, q) + 1;
    return true;
}

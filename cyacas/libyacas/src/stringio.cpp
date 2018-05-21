#include "yacas/stringio.h"

StringInput::StringInput(const std::string& aString, InputStatus& aStatus) :
    LispInput(aStatus),
    _string(aString),
    _current(_string.begin())
{
}

char32_t StringInput::Next()
{
    if (EndOfStream())
        return std::char_traits<char32_t>::eof();

    const char32_t cp =
        utf8::next(_current, std::string::const_iterator(_string.end()));

    if (cp == '\n')
        iStatus.NextLine();

    return cp;
}

char32_t StringInput::Peek()
{
    if (EndOfStream())
        return std::char_traits<char32_t>::eof();

    return utf8::peek_next(_current,
                           std::string::const_iterator(_string.end()));
}

bool StringInput::EndOfStream() const
{
    return _current == _string.end();
}

std::size_t StringInput::Position() const
{
    return utf8::distance(_string.begin(), _current);
}

void StringInput::SetPosition(std::size_t n)
{
    _current = _string.begin();
    utf8::advance(_current, n, std::string::const_iterator(_string.end()));
}

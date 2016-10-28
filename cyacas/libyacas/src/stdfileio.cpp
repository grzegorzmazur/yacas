#include "yacas/platfileio.h"

#ifdef _WIN32
#define MAP_TO_WIN32_PATH_SEPARATOR
#endif // WIN32

static void MapPathSeparators(std::string& filename)
{
#ifdef MAP_TO_WIN32_PATH_SEPARATOR
    for (std::size_t i = 0; i < filename.size(); ++i)
        if (filename[i] == '/')
            filename[i] = '\\';
#endif
}


StdFileInput::StdFileInput(std::istream& stream, InputStatus& aStatus):
    LispInput(aStatus),
    _stream(stream)
{
}

StdFileInput::StdFileInput(LispLocalFile& file, InputStatus& aStatus):
    LispInput(aStatus),
    _stream(file.stream),
    _position(0),
    _cp_ready(false)
{
}

char32_t StdFileInput::Next()
{
    if (!_cp_ready)
        _get();

    if (EndOfStream())
        return std::char_traits<char32_t>::eof();
    
    _cp_ready = false;
    _position += 1;
    
    return _cp;
}

char32_t StdFileInput::Peek()
{
    if (EndOfStream())
        return std::char_traits<char32_t>::eof();

    if (!_cp_ready)
        _get();
    
    return _cp;
}

void StdFileInput::Rewind()
{
    _stream.seekg(0);
    _position = 0;
    _cp_ready = false;
}

bool StdFileInput::EndOfStream() const
{
    if (_stream.eof())
        return true;
    
    if (!_cp_ready)
        _get();
            
    return _stream.eof();
}

std::size_t StdFileInput::Position() const
{
    return _position;
}

void StdFileInput::SetPosition(std::size_t n)
{
    Rewind();
    
    for (std::size_t i = 0; i < n; ++i)
        Next();
}

void StdFileInput::_get() const
{
    char p[4];
    char* q = p;
    
    *q++ = _stream.get();
    
    while (!_stream.eof() && !utf8::is_valid(p, q))
        *q++ = _stream.get();
    
    if (_stream.eof())
        return;
    
    utf8::utf8to32(p, q, &_cp);
    _cp_ready = true;
}


std::string InternalFindFile(const char* fname, const std::vector<std::string>& dirs)
{
    std::string path(fname);

    MapPathSeparators(path);

    FILE* file = fopen(path.c_str(), "rb");
    for (std::size_t i = 0; !file && i < dirs.size(); ++i) {
        path = dirs[i];
        path += fname;
        MapPathSeparators(path);
        file = fopen(path.c_str(), "rb");
    }

    if (file)
        fclose(file);

    if (!file)
        return "";

    return path;
}

LispLocalFile::LispLocalFile(
    LispEnvironment& environment,
    const std::string& fname,
    bool read,
    const std::vector<std::string>& dirs):
    environment(environment)
{
    std::string othername;

    if (read) {
        othername = fname;
        MapPathSeparators(othername);

        stream.open(othername, std::ios_base::in | std::ios_base::binary);

        for (std::size_t i = 0; !stream.is_open() && i < dirs.size(); ++i) {
            othername = dirs[i];
            othername += fname;
            MapPathSeparators(othername);
            stream.open(othername, std::ios_base::in | std::ios_base::binary);
        }
    } else {
        othername = fname;
        MapPathSeparators(othername);
        stream.open(othername, std::ios_base::out);
    }
}

LispLocalFile::~LispLocalFile()
{
    if (stream.is_open())
        stream.close();
}

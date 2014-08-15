#include <stdlib.h>
#include <cctype>
#include <cstring>
#include <iostream>
#include <map>
#include <string>

typedef std::map<std::string, std::string> CmdEntries;


CmdEntries commands;
CmdEntries files;

int fileline = 0;

void GetBf(char* buf, int size, FILE*f)
{
    if (!fgets(buf, 2048, f)) {
        *buf = '\0';
        if (!feof(f)) {
            std::cerr << "Error reading, bailing out.\n";
            exit(-1);
        }
    }

    fileline++;
    const std::size_t n = std::strlen(buf);
    if (n && buf[n - 1] == '\n')
        buf[n] = '\0';
    char* ptr = buf;
    while (*ptr != '\0') {
        if (*ptr == ':') *ptr = ' ';
        ptr++;
    }
}

void ProcessFile(const char* fname)
{
    std::cerr << "\tFILE " << fname << "\n";
    FILE*f = fopen(fname, "r");
    if (!f) return;
    int prevline = fileline;
    fileline = 0;
    char buf[2048];
    while (!feof(f)) {
        GetBf(buf, 2048, f);
        const std::size_t n = std::strlen(buf);
        if (n && buf[n - 1] == '\n')
            buf[n - 1] = '\0';

        //*CMD Sin, Cos, Tan --- trigonometric functions
        if (!strncmp(buf, "*CMD", 4)) {
            char* description = buf;
            while (*description != '\0' && *description != '-') description++;
            if (*description == '\0') continue;
            while (*description == '-') description++;
            while (*description == ' ') description++;

            char* command = &buf[5];
            for (;;) {
                char* end = command;
                while (*end != ',' && *end != ' ') end++;
                char cmd[256];
                memset(cmd, 0, 255);
                memcpy(cmd, command, end - command);
                //        cmd[end-command] = '\0';

                commands[cmd] = description;
                files[cmd] = fname;

                if (*end == ' ') break;
                end++;
                if (*end == ' ') end++;
                command = end;
            }
        } else if (!strncmp(buf, "*CALL", 5)) {
            while (buf[0] != '\t')
                GetBf(buf, 2048, f);
            char cmd[256];
            while (buf[0] == '\t') {
                if (buf[strlen(buf) - 1] == '\n') buf[strlen(buf) - 1] = '\0';
                char* ptr = &buf[1];
                while (std::isalnum(*ptr) || *ptr == '\'') ptr++;
                memset(cmd, 0, 255);
                memcpy(cmd, &buf[1], ptr - (&buf[1]));

                if (cmd[0] && strncmp(&buf[1], "In>", 3) && strncmp(&buf[1], "Out>", 4) && isupper(buf[1])) {
                    CmdEntries::const_iterator iter = commands.find(cmd);
                    const char* desc = "";
                    if (iter != commands.end())
                        desc = iter->second.c_str();
                    else
                        std::cerr << "WARNING: function " << cmd << " does not have a short description (" << fname << ":" << fileline << ")\n";

                    std::cout << ":" << cmd << ":" << &buf[1] << ":" << desc << ":\n";
                    commands[cmd] = "";
                }
                GetBf(buf, 2048, f);
                while (buf[0] == '\0') GetBf(buf, 2048, f);
            }
            if (buf[0] == ' ')
                std::cerr << "WARNING: spaces might have been used instead of tabs (" << cmd << ", " << fname << ":" << fileline << ")\n";
        } else if (!strncmp(buf, "*INCLUDE", 8)) {
            const char* fn = &buf[8];
            while (*fn == ' ')
                ++fn;
            std::string fullname = fn;
            fullname += ".txt";
            ProcessFile(fullname.c_str());

        }
    }
    fclose(f);
    fileline = prevline;
}

int main(int argc, char** argv)
{
    for (int i = 1; i < argc; ++i) {
        std::cerr << "BOOK " << argv[i] << "\n";
        ProcessFile(argv[i]);
    }

    for (CmdEntries::const_iterator iter = commands.begin(); iter != commands.end(); ++iter) {
        if (iter->second.c_str()[0]) {
            CmdEntries::const_iterator fiter = files.find(iter->first.c_str());
            if (fiter != files.end())
                std::cerr << "WARNING: no call sequences for function " << iter->first << " (" << fiter->second << ")\n";
            else
                std::cerr << "WARNING: no call sequences for function " << iter->first << "\n";
        }
    }
}

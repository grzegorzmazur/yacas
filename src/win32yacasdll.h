
// The following ifdef block is the standard way of creating macros which make exporting
// from a DLL simpler. All files within this DLL are compiled with the YACASDLL_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see
// YACASDLL_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef YACASDLL_EXPORTS
#define YACASDLL_API __declspec(dllexport)
#else
#define YACASDLL_API __declspec(dllimport)
#endif

// This class is exported from the yacasdll.dll
class YACASDLL_API CYacasdll {
public:
  CYacasdll(void);
  // TODO: add your methods here.
};



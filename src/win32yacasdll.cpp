// yacasdll.cpp : Defines the entry point for the DLL application.
//

// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__E53BA513_A2C9_11D6_916F_00C04F89BA91__INCLUDED_)
#define AFX_STDAFX_H__E53BA513_A2C9_11D6_916F_00C04F89BA91__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


// Insert your headers here
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

#include <windows.h>

// TODO: reference additional headers your program requires here

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__E53BA513_A2C9_11D6_916F_00C04F89BA91__INCLUDED_)

#include "win32yacasdll.h"

HANDLE hThisModule = NULL;
BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
  hThisModule = hModule;
  switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
    }
    return TRUE;
}

// This is the constructor of a class that has been exported.
// see yacasdll.h for the class definition
CYacasdll::CYacasdll()
{ 
	return; 
}


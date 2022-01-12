// DemoC++Plugin.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include <windows.h>
#include <stdlib.h>

#define DLLEXPORT __declspec(dllexport)

/*********************************************************
 *         SmartieInit                                   *
 *********************************************************/
extern "C" DLLEXPORT  void 
__stdcall  SmartieInit()
{
	// This function will be called when the plugin is 1st loaded.
	// This function is OPTIONAL.
}

/*********************************************************
 *         SmartieFini                                   *
 *********************************************************/
extern "C" DLLEXPORT  void 
__stdcall  SmartieFini()
{
	// This function will be called just before the plugin
	// is unloaded. This function is OPTIONAL.
}

/*********************************************************
 *         GetMinRefreshInterval                         *
 *********************************************************/
extern "C" DLLEXPORT  int
__stdcall  GetMinRefreshInterval()
{
	//
	// Define the minimum interval that a screen should get fresh
	// data from our plugin.
	// The actual value used by Smartie will be the higher of this
	// value and of the "dll check interval" setting
	// on the Misc tab.  [This function is optional, Smartie will
	// assume 300ms if it is not provided.]
	// 
	return 300; // 300 ms
}


/*********************************************************
 *         Function 1                                    *
 *  Simply returns "This is function 1"				     *
 *********************************************************/
extern "C" DLLEXPORT  char * 
__stdcall  function1(char *param1, char *param2)
{
    return "This is function 1";       
}


/*********************************************************
 *         Function 2                                    *
 *  Returns how many times the function has been called. *
 *********************************************************/
extern "C" DLLEXPORT  char * 
__stdcall  function2(char *param1, char *param2)
{
    static char outbuf[1000];
    static int count;
    
    count ++;
    
    itoa(count, outbuf, 10);
    return outbuf;       
}
    

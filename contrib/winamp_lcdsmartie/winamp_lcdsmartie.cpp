//
// This file is based on code from lcd plugin.
//
//
/////////////////////////////////////////////////////////////////////////////
//  Copyright (C) 2001 Frank Verhamme. All Rights Reserved.
//
//  This file is part of the WinAmp Serial LCD Display Plugin.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
//  USA.
//
/////////////////////////////////////////////////////////////////////////////


#include "stdafx.h"
#include <stdio.h>
#include "winamp_lcdsmartie.h"

// returns a winampVisModule when requested. Used in hdr, below
winampVisModule *getModule(int which);

// "member" functions
static void config(struct winampVisModule *this_mod); // configuration dialog

static int init(struct winampVisModule *this_mod);	   // initialization for module
static void quit(struct winampVisModule *this_mod);   // deinitialization for module
static int renderLinearSA(struct winampVisModule *this_mod);  // rendering for module
static int renderTest(struct winampVisModule *this_mod);  // rendering for module

// Module header, includes version, description, and address of the module retriever function
winampVisHeader hdr = { VIS_HDRVER, "Spectrum Analyser for LCDSmartie", getModule };

winampVisModule mod1 =
{
	"Spectrum Analyser",
	NULL,   // hwndParent
	NULL,   // hDllInstance
	0,      // sRate
	0,      // nCh
	25,     // latencyMS
	25,     // delayMS
	2,      // spectrumNch
	0,      // waveformNch
	{ 0, },	// spectrumData
	{ 0, },	// waveformData
	config,
	init,
	renderLinearSA, 
	quit
};


winampVisModule mod2 =
{
	"Test",
	NULL,   // hwndParent
	NULL,   // hDllInstance
	0,      // sRate
	0,      // nCh
	25,     // latencyMS
	25,     // delayMS
	2,      // spectrumNch
	0,      // waveformNch
	{ 0, },	// spectrumData
	{ 0, },	// waveformData
	config,
	init,
	renderTest, 
	quit
};

// These are for our shared memory
static HANDLE hMapFile;
static LPCTSTR pBuf;
static shared *sharedMem;

// this is the only exported symbol. returns our main header.
// if you are compiling C++, the extern "C" { is necessary, so we just #ifdef it
#ifdef __cplusplus
extern "C" {
#endif
__declspec( dllexport ) winampVisHeader *winampVisGetHeader()
{
  return &hdr;
}
#ifdef __cplusplus
}
#endif

// getmodule routine from the main header. Returns NULL if an invalid module was requested,
// otherwise returns either mod1 or mod2 depending on 'which'.s
winampVisModule *getModule(int which)
{
  switch (which)
	{
	    case 0: return &mod1;
		case 1: return &mod2;
		default: return NULL;
	}
}

// configuration.
// Passed this_mod, as a "this" parameter.
// Allows you to make one configuration function
// that shares code for all your modules
void config(struct winampVisModule *this_mod)
{
	MessageBox(this_mod->hwndParent,"LCD Smartie winamp plugin\n"
									"No further configuration needed.",
									"Configuration",MB_OK);
}


// initialization.
// returns 0 on success, 1 on failure.
int init(struct winampVisModule *this_mod)
{
	hMapFile = CreateFileMapping(
                 INVALID_HANDLE_VALUE,    // use paging file
                 NULL,                    // default security 
                 PAGE_READWRITE,          // read/write access
                 0,                       // max. object size 
                 sizeof(shared),          // buffer size  
                 "LCDSmartieWinAmpPlugin");   // name of mapping object
 
   if (hMapFile == NULL || hMapFile == INVALID_HANDLE_VALUE) 
   { 
      //printf("Could not create file mapping object (%d).\n", GetLastError());
      return 1;
   }
   
   pBuf = (LPTSTR) MapViewOfFile(hMapFile,   // handle to mapping object
                        FILE_MAP_ALL_ACCESS, // read/write permission
                        0,                   
                        0,                   
                        sizeof(shared));           
 
   if (pBuf == NULL) 
   { 
      //printf("Could not map view of file (%d).\n", GetLastError()); 
      return 1;
   }


	sharedMem = (shared *)pBuf;
	return 0;
}

/******************************************************************************
Function : renderLinearSA
Purpose  : Renders the linear spectrum analyser
Parameters : pointer to visualisation module
Returns : 0 if successful, 1 if visualization should end.
******************************************************************************/
int renderLinearSA(struct winampVisModule *this_mod)
{	
	sharedMem->dataType = 0;

	memcpy(&sharedMem->data, this_mod->spectrumData, sizeof(this_mod->spectrumData));

	return 0;
}

int renderTest(struct winampVisModule *this_mod)
{	
	sharedMem->dataType = 0;
	static int i = 0;
	static int j = 0;

	i++;
	if (i>1)
	{
		i=0;

		j++;
		if (j>576)
			j=0;
		memset(&sharedMem->data, 0, sizeof(this_mod->spectrumData));
		sharedMem->data[0][j] = 255;
		sharedMem->data[1][j] = 255;
	}

	return 0;
}

// quit.
// Destroys the window, unregisters the window class
void quit(struct winampVisModule *this_mod)
{
   if (pBuf) UnmapViewOfFile(pBuf);
   pBuf = NULL;
   if ((hMapFile != NULL) && (hMapFile != INVALID_HANDLE_VALUE)) CloseHandle(hMapFile);
   hMapFile = NULL;
   sharedMem = NULL;
}


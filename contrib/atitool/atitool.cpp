/////////////////////////////////////////////////////////////////////////////
//
//  This file is part of the LCDSmartie ATI Tool Plugin.
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
#include "windows.h"
#include "atitool.h"
#include <string>

using namespace std;

#define SH_MEM_NAME "ATIToolData" 
 
/* Defined in Kernel mode driver - stored here for reference */
enum TEMPCHIPS {  
	NOTSUPPORTED,	// Card does not support temperature monitoring 
	NOTDETECTED,	// Card could support temp. monitoring, but no chip detected 
	LM63,			// LM63 detected 
	ASUS			// ASUS temp. chip detected 
}; 

 
struct CARDINFO { 
	UINT32 deviceID; 
	wchar_t chipType[15]; // UNICODE characters .. 2 Bytes wide 
	UINT32 memSize; 
	UINT16 busWidth; 
	UINT16 numPipes; 
	double core; // Core frequency 
	double mem; // Memory frequency 
	TEMPCHIPS tempChip; 
	BOOL is3dModeActive; 
	double tempLocal; // Local temp., usually temp. of Temperature Chip, NaN = unknown 
	double tempRemote; // Remote temp., usually temp. of GPU, NaN = unknown 
	double tempLocalMin; 
	double tempLocalMax; 
	double tempLocalAvg; 
	long tempLocalMeasurements; 
	double tempRemoteMin; 
	double tempRemoteMax; 
	double tempRemoteAvg; 
	long tempRemoteMeasurements; 
}; 

HANDLE SharedMemHandle;
CARDINFO *SharedMemory;


extern "C" ATITOOL_API  void
__stdcall  SmartieInit()
{
}


extern "C" ATITOOL_API  void
__stdcall  SmartieFini()
{
	if (SharedMemHandle > 0)
	{
	    if (SharedMemory)
		{
			UnmapViewOfFile(SharedMemory);
			SharedMemory = NULL;
		}
		CloseHandle(SharedMemHandle);
		SharedMemHandle = 0;
	}
}

bool MapMemory()
{
	if ((SharedMemHandle > 0) && (SharedMemory)) return true;
	else
	{

		SharedMemHandle = OpenFileMapping(FILE_MAP_READ, false, SH_MEM_NAME);
		if (SharedMemHandle > 0)
		{
			SharedMemory = (CARDINFO *)MapViewOfFile(SharedMemHandle, FILE_MAP_READ, 0, 0, 0);
			if (SharedMemory) return true;
		}
		else
		{
			CloseHandle(SharedMemHandle);
			SharedMemHandle = 0;
		}
	}
	return false; 
}


/*******************************************************************
 *         Function 1                                              *
 *******************************************************************/
extern "C" ATITOOL_API  char * 
__stdcall  function1(const char *param1, const char *param2)
{ 
	static string outbuf;

	if (MapMemory())
	{
		char temp[1000];

		int function = atoi(param1);
		int precision = atoi(param2);
		switch (function)
		{
		case 1:
			if (SharedMemory->is3dModeActive) outbuf = "3D";
			else outbuf = "2D";
			break;
		case 2:
			sprintf(temp, "%.*f", precision, SharedMemory->tempLocal);
			outbuf = temp;
			break;
		case 3:
			sprintf(temp, "%.*f", precision, SharedMemory->tempRemote);
			outbuf = temp;
			break;
		case 4:
			sprintf(temp, "%.*f", precision, SharedMemory->tempLocalMin);
			outbuf = temp;
			break;
		case 5:
			sprintf(temp, "%.*f", precision, SharedMemory->tempLocalMax);
			outbuf = temp;
			break;
		case 6:
			sprintf(temp, "%.*f", precision, SharedMemory->tempLocalAvg);
			outbuf = temp;
			break;
		case 7:
			sprintf(temp, "%.*f", precision, SharedMemory->tempRemoteMin);
			outbuf = temp;
			break;
		case 8:
			sprintf(temp, "%.*f", precision, SharedMemory->tempRemoteMax);
			outbuf = temp;
			break;
		case 9:
			sprintf(temp, "%.*f", precision, SharedMemory->tempRemoteAvg);
			outbuf = temp;
			break;
		case 10:
			sprintf(temp, "%.*f", precision, SharedMemory->core);
			outbuf = temp;
			break;
		case 11:
			sprintf(temp, "%.*f", precision, SharedMemory->mem);
			outbuf = temp;
			break;
		}
	}
	else
		outbuf = "atitool not running";


	return const_cast<char *>(outbuf.c_str());
}

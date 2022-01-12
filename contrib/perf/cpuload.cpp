/////////////////////////////////////////////////////////////////////////////
//  
//  This file is part of the LCDSmartie perf Plugin.
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
#include <cstdlib>
#include <Pdh.h>
#include <PdhMsg.h>
#include <vector>
#include "cpuload.h"
#include "lock.h"
#include <iostream>
#include <cstdlib>
#include <process.h>
#include "assert.h"
#include "PDHMsg.h"

#undef PdhOpenQuery   //       PdhOpenQueryA or PdhOpenQueryW
extern "C" long __stdcall 
PdhOpenQuery (LPCSTR  szD, DWORD  dw, HQUERY  *phQ );

using namespace std;

string CpuLoad::errMsg(DWORD errCode)
{
	LPVOID buf;
	string sError;

	if (errCode != 0)
	{
		char sErrCode[100];

		if (FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM
			| FORMAT_MESSAGE_FROM_HMODULE,
			GetModuleHandle( TEXT("PDH.DLL") ), errCode,
			MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&buf, 0, 0 ) == 0)
			buf = 0;

		itoa(errCode, sErrCode, 16);
		if (buf != 0)
		{
			sError = "#" + string(sErrCode) + ": " + string((LPTSTR)buf);
			LocalFree(buf);
		}
		else
		{
			sError = "#" + string(sErrCode);
		}
	}
	else
		sError = "#0"; // don't put "operation completed successfully!" It's too confusing!

	return sError;
}

unsigned int WINAPI 
CpuLoad::MyThreadProc( LPVOID pParam )
{
    CpuLoad* pObject = (CpuLoad*)pParam;

    if (pObject == NULL)
      return 1;   // if pObject is not valid

    pObject->Collector();

    _endthreadex( 0 );
    return 0;   // thread completed successfully
}



void
CpuLoad::Collector()
{
    PDH_STATUS status;
    PDH_FMT_COUNTERVALUE value;
    
    while (!exiting)
	{			          
          status = PdhCollectQueryData(query);
          if (status !=  ERROR_SUCCESS)
		  {
              //throw string("[PdhCollectQueryData failed]");
		  } else {    
			status = PdhGetFormattedCounterValue(*counter, 
				  PDH_FMT_LONG | PDH_FMT_NOSCALE | PDH_FMT_NOCAP100,
				NULL,
				&value);
			if (status !=  ERROR_SUCCESS)
			{
				//throw string("[PdhGetFormattedCounterValue failed]");
			} else if ((value.CStatus != PDH_CSTATUS_NEW_DATA) &&
				(value.CStatus != PDH_CSTATUS_VALID_DATA))
			{
				//throw string("[Invalid data]");
			} else  {
				Lock cs(dataCS);
	          
				dataIn = (dataIn + 1) % dataSize;
				data[dataIn] = value.longValue;
			}
		  }

		  
		  if (WaitForSingleObject(stopEvent, sampleTime) == WAIT_OBJECT_0)
			  break;
    }
	SetEvent(stoppedEvent);
}

CpuLoad::CpuLoad(int dataitems, int sampleTime, string& counterName)
{
    PDH_STATUS status;
    
    status = PdhOpenQuery(NULL, 0, &query);
    if (status !=  ERROR_SUCCESS)
       throw string("[PdhOpenQuery failed]");
        
	counter = (HCOUNTER *)GlobalAlloc(GPTR, sizeof(HCOUNTER));
	if (counter == NULL)
		throw string("[No Memory]");

    status = PdhAddCounter(query, counterName.c_str(), 0, counter);
    if (status !=  ERROR_SUCCESS)
       throw string("[Bad Counter" + errMsg(status) + "]");

	// The following calls are just to check that everything is going to work
	// in the collector thread...
    status = PdhCollectQueryData(query);
    if (status !=  ERROR_SUCCESS)
		throw "[PdhCollectQueryData Failed]";

	PDH_FMT_COUNTERVALUE value;
    status = PdhGetFormattedCounterValue(*counter, 
          PDH_FMT_LONG | PDH_FMT_NOSCALE | PDH_FMT_NOCAP100,
          NULL,
          &value);
    if (status !=  ERROR_SUCCESS)
		throw string("[Bad counter: " + errMsg(status) + "]");

    if ((value.CStatus != PDH_CSTATUS_NEW_DATA) &&
         (value.CStatus != PDH_CSTATUS_VALID_DATA))
          throw string("[Invalid data]");
          

    InitializeCriticalSection(&dataCS);
	stopEvent = CreateEvent(NULL, true, false, NULL);
	stoppedEvent = CreateEvent(NULL, true, false, NULL);

    dataSize = dataitems;
    data = (unsigned int *)malloc(dataSize * sizeof(unsigned int));
    dataIn = 0;
    this->sampleTime = sampleTime;
    exiting = false;
     
    memset(data, 0, dataSize*sizeof(unsigned int));   
        
    unsigned int tid;
    thread = (HANDLE)_beginthreadex(NULL, 0, &MyThreadProc, this, 0, &tid);
	// check for success
    
}

CpuLoad::~CpuLoad()
{
     PDH_STATUS status;

     exiting = true;
	 SetEvent(stopEvent);
	 // If we're being closed in DllMain then the thread will never be signalled,
	 // because the OS is holding a mutex to serialize access to DllMain (which is needed 
	 // when a thread exits).
     WaitForSingleObject(stoppedEvent, INFINITE);
	 if (WaitForSingleObject(thread, 5) == WAIT_OBJECT_0)
		CloseHandle(thread);

	 status = PdhCloseQuery(query);
     if (status !=  ERROR_SUCCESS) 
	 {	 // can fail with a timeout.
	 	throw string("PdhCloseQuery failed");   
	 }
	 else if (counter != NULL)
	 {
		GlobalFree(counter);
	 }

     if (data)
     {
		free(data);
        data = NULL;
        DeleteCriticalSection(&dataCS);
		CloseHandle(stopEvent);
		CloseHandle(stoppedEvent);
     }

}

unsigned int
CpuLoad::GetCpuUsage()
{    
     unsigned int load;             
     Lock cs(dataCS);

     load = data[dataIn]; 

     return load;
}

void
CpuLoad::GetCpuUsages(unsigned int numValues, vector<unsigned int>& values)
{    
     unsigned int count;
     unsigned int start;            
     Lock cs(dataCS);

     values.resize(numValues);

     start = (dataIn + dataSize - numValues + 1) % dataSize;
     
     for (count = 0; count < numValues; count++)
     {
          values[count] = data[start];
          start = (start +1) % dataSize;
     }   
}


 

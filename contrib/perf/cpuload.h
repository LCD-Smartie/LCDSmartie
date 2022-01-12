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
#ifndef G__CPULOAD_H__
#define G__CPULOAD_H__

#include <Pdh.h>
#include <PdhMsg.h>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>




class CpuLoad
{
    public:          
		   CpuLoad(int dataitems, int sampleTime, std::string& counterName);
           ~CpuLoad();
           unsigned int GetSize(){ return dataSize; };
           unsigned int GetCpuUsage();
		   void GetCpuUsages(unsigned int numValues, std::vector<unsigned int>& values);
    private:
		   CpuLoad(const CpuLoad&);
		   CpuLoad& operator=(const CpuLoad&);
           PDH_HQUERY query;
           PDH_HCOUNTER *counter;
           static unsigned int WINAPI MyThreadProc( LPVOID pParam );
		   std::string errMsg(DWORD errCode);
           void Collector();
           HANDLE thread;
           bool exiting;
           CRITICAL_SECTION dataCS;
		   HANDLE stopEvent;
		   HANDLE stoppedEvent;
           unsigned int sampleTime;
           unsigned int *data;
           unsigned int dataSize;
           unsigned int dataIn;
};




#endif // G__CPULOAD_H__

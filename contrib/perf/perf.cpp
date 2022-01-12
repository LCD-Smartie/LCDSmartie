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
// perf.cpp : Defines the entry point for the DLL application.
//
#include "stdafx.h"
#include <vector>
#include <map>
#include "cpuload.h"
#include "graph.h"
#include "perf.h"
#include "assert.h"

using namespace std;

class GraphObj : public Graph
{		
	public:
		GraphObj(string params, unsigned int height, unsigned int width, unsigned int time,
			string& counterName, Direction direction, unsigned int barStyle, unsigned int minVal,
			unsigned int maxVal) 
			: Graph(height, width, time, counterName, direction, barStyle, minVal, maxVal),
			createParams(params) { };
		string createParams;
};


// global store of graph objects
typedef map<string, GraphObj> GraphMap;
GraphMap graphs;

extern "C" PERF_API  void
__stdcall  SmartieFini()
{
	graphs.clear();
}

extern "C" PERF_API  int 
__stdcall  GetMinRefreshInterval()
{
	return 100; // 100ms is surely quick enough
}


bool
JustDigits(char *str)
{
	while (*str && *str != '#')
	{
		if (*str < '0' || *str > '9')
			return false;
		str++;
	}
	return true;
}

Graph&
findOrCreateGraph(const char *param1, const char *param2)
{
	string graphName(param2);

	// check if we have seen this graph definition before
	GraphMap::iterator graphIter = graphs.find(graphName);

    if (graphIter != graphs.end())
	{
		assert(graphIter->first == graphName);
		if (graphIter->second.createParams == param1)
			return graphIter->second;

		// inital parameters have changed - delete the old object and create a new one.
		graphs.erase(graphIter);
	}
           
    // otherwise create the object  

	
        // ====== Parse parameters ===========
		// param1 is options and takes the form:
		//   HeightxWidth#Direction#BarStyle#SampleTime#min#max#CounterName
		// where:
		//   HeightxWidth is the size of the bar graph
		//   Direction is one of: d, u, l, r   [for down/up/left/right]
		//   SampleTime is how often the counter value is fetched, in 1/10 seconds.
		//   BarStyle selects the custom characters to use
		//   CounterName is a name of a performance counter (ie. '\Processor(0)\% Processor Time')
	    //   min - used for scaling graph.
	    //   max - used for scaling graph.
		// param2 is a name to use in function2 to get further lines.
	    
		// handle HeightxWidth
		unsigned int height = atoi(param1); 
		const char *x = strchr(param1, 'x');
        if (x == NULL)
           throw "[HEIGHTxWIDTH required]";
		unsigned int width = atoi(x+1);
		if ((width < 1) || (width > 40))
			throw "[Width out of range]";
		if ((height < 1) || (height > 4))
			throw "[Height out of range]";

		// handle Direction
		Graph::Direction direction;
		const char *hash = strchr(x, '#');
		if (hash != NULL)
		{
			hash ++;
			switch (*hash)
			{
			case 'l': direction = Graph::LEFT; break;
			case 'r': direction = Graph::RIGHT; break;
			case 'u': direction = Graph::UP; break;
			case 'd': direction = Graph::DOWN; break;
			default: throw "[Invalid direction]";
			}
		} 
		else
			direction = Graph::UP;

		// handle BarStyle
		unsigned int barStyle;
		if (hash != NULL)
			hash = strchr(hash, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[perf: bad bar style]";
			barStyle = atoi(hash);
		}
		else
			barStyle = 1;
		

		// handle SampleTime
		unsigned int sampleTime;
		if (hash != NULL)
			hash = strchr(hash, '#');

		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[perf: bad sample time]";
			sampleTime = atoi(hash) * 100; // convert to mSec

			if (sampleTime < 1)
				throw "[Invalid Sample Time]";
		} 
		else
			sampleTime = 500; 
		


		// handle min
		unsigned int min;
		if (hash != NULL)
			hash = strchr(hash, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[perf: bad min]";
			min = atoi(hash);
		}
		else
			min = 0;

		// handle max
		unsigned int max;
		if (hash != NULL)
			hash = strchr(hash, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[perf: bad max]";
			max = atoi(hash);
		}
		else
			max = 100;
				
		// handle counterName
		char *counterName;
		if (hash != NULL)
			hash = strchr(hash, '#');

		if (hash != NULL)
		{
			hash ++;
			counterName = const_cast<char*>(hash);
		}
		else
			counterName = "\\Processor(0)\\% Processor Time";

		
		// Convert our barStyle to that used by graph
		switch (barStyle)
		{
		case 0: break;
		case 1: break;
		case 2: barStyle |= SIXWIDE; break;
		case 3: barStyle |= SIXWIDE | SEVENHIGH; break;
		case 4: barStyle |= SEVENHIGH; break;
		case 5: break;
		case 9: barStyle = 6 | SEVENHIGH; break;
		case 13: barStyle = 7 | SEVENHIGH; break;
		case 99: barStyle = TINY; break;
		case 100: barStyle = TINY | SIXWIDE; break;
		case 101: barStyle = TINY | SIXWIDE | SEVENHIGH; break;
		case 102: barStyle = TINY | SEVENHIGH; break;
		default:
			throw "[perf: unknown bar style]";
		}


		if (barStyle & TINY) // special bar style for small (1x8 or less) graphs
		{
			if (height > 1)
				throw "[Graph too high for tiny graphs]";
			if (width > 8)
				throw "[Graph too wide for tiny graphs]";

			if (barStyle & SIXWIDE)
				width *= 6;
			else
				width *= 5; // there are 5 pixels in a custom chars.
		}

		
		graphs.insert(pair<string, GraphObj>(graphName, 
			GraphObj(param1, height, width, sampleTime, string(counterName), direction, barStyle, min, max)));
		GraphMap::iterator graphIter2 = graphs.find(graphName);
		assert(graphIter2 != graphs.end());
		return graphIter2->second;
}

   
/*********************************************************
 *         Function 1                                    *
 * Defines a graphing object and returns it's first line.*
 *  param1 - options for graphing object.                *
 *  param2 - name given to graphing object.              *
 *********************************************************/
extern "C" PERF_API  char * 
__stdcall  function1(const char *param1, const char *param2)
{ 
	static string outbuf;

    try
    {
		// ====== Find object or create a new one ===========
		Graph& graph = findOrCreateGraph(param1, param2);
    
		// get top line of the graph
		graph.BarGraph(graph.GetHeight(), outbuf);
        
	} catch (char *e) {
		outbuf = e;
	} catch (string s) {
		outbuf = s;
	} catch(...) {
        outbuf = "Exception";
    }

	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 2                                    *
 * Returns a line from a pre-existing graphing object.   *
 *  graphName - name of graphing object to use.          *
 *  lineNum - line number to return                      *
 *********************************************************/
extern "C" PERF_API  char * 
__stdcall  function2(const char *graphName, const char *lineNum)
{ 
	static string outbuf;

    try
    {
		// ====== Find object or create a new one ===========
		GraphMap::iterator graphIter = graphs.find(graphName);

		if (graphIter != graphs.end())
		{
			int line = atoi(lineNum);
			if ((line > 0) && (line <= 4))
				graphIter->second.BarGraph(line, outbuf); // get line one of the graph
			else
				outbuf = "[bad line number]";
		}
		else
		{
			outbuf = "[No such graph]";
		}
        
	} catch (char *e) {
		outbuf = e;
	} catch (string s) {
		outbuf = s;
	} catch(...) {
        outbuf = "Exception";
    }

	return const_cast<char *>(outbuf.c_str());
}



/*********************************************************
 *         Function 4                                    *
 * Returns the current value of a proformance counter.   *
 *  param1 - Sample time.                                *
 *  param2 - Counter name.                               *
 *********************************************************/
extern "C" PERF_API  char * 
__stdcall  function4(const char *param1, const char *param2)
{ 
	static string outbuf;

    try
    {
		string fakeOptions("1x1#u#1#");
		fakeOptions += string(param1) + "#0#1#" + string(param2);
		// ====== Find object or create a new one ===========
		Graph& graph = findOrCreateGraph(fakeOptions.c_str(), fakeOptions.c_str());
             
		char c[100];
		itoa(graph.GetCurrentValue(), c, 10);
    
		outbuf = c;
	} catch (char *e) {
		outbuf = e;
	} catch (string s) {
		outbuf = s;
	} catch(...) {
        outbuf = "Exception";
    }

	return const_cast<char *>(outbuf.c_str());
}

extern "C" PERF_API  char * 
__stdcall  function8(const char *param1, const char *param2)
{
	Sleep(10000);
	return "";
}

extern "C" PERF_API  char * 
__stdcall  function9(const char *param1, const char *param2)
{
	static int i = 0;
	i = 1 - i;
	if (i == 0)
		return "0";
	else
		return "1";
}


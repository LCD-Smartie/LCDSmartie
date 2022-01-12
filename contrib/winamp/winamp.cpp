// Some code in this file is based on code from lcd plugin (a winamp plugin).
// That code was Copyright (C) 2000 Frank Verhamme, and JMattila
/////////////////////////////////////////////////////////////////////////////
//
//  This file is part of the LCDSmartie Winamp Plugin.
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
#include "winamp.h"
#include <math.h>
#include <string>
#include <vector>
#include "graph.h"

using namespace std;

namespace {
	HANDLE hMapFile;
	LPCTSTR pBuf;
	shared *sharedMem;
	void renderNonLinearSA();
	void renderLinearSA();
	int AdjustFalloff(int x, double value);
	void parseParameters(const char *param1, const char *param2);
	bool applyFalloff = true;
	bool applyAWeighting = true;
	bool applyNonLinear = true;
	double  darrFalloff[48];
	vector<unsigned int> spectrumValues;
	double  dFalloffValue = 20;
	Graph *currGraph = 0;
	unsigned int width, height;
};

extern "C" WINAMP_API  void
__stdcall SmartieInit()
{
	hMapFile = CreateFileMapping(
            INVALID_HANDLE_VALUE,    // use paging file
            NULL,                    // default security 
            PAGE_READWRITE,          // read/write access
            0,                       // max. object size 
            sizeof(shared),          // buffer size  
            "LCDSmartieWinAmpPlugin");   // name of mapping object


	if (hMapFile != NULL) 
	{ 
		
		pBuf = (LPCTSTR) MapViewOfFile(hMapFile,    // handle to mapping object
			FILE_MAP_ALL_ACCESS,  // read/write permission
			0,                    
			0,                    
			sizeof(shared));                   

		if (!pBuf) 
		{ 
			//printf("Could not map view of file (%d).\n", GetLastError()); 
			//return;
		}

		sharedMem = (shared *)pBuf;
	}

}

extern "C" WINAMP_API  void
__stdcall SmartieFini()
{
	if (pBuf) UnmapViewOfFile(pBuf);
	pBuf = NULL;
	sharedMem = NULL;
	if ((hMapFile != NULL) && (hMapFile != INVALID_HANDLE_VALUE))
		CloseHandle(hMapFile);
	hMapFile = NULL;
}

extern "C" WINAMP_API  int
__stdcall  GetMinRefreshInterval()
{
	return 12; // 12 ms is as low as makes sense (half of 25ms which is how oftern the spectrum data is stored).
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


/*********************************************************
 *         Function 1                                    *
 * Draws a spectrum analyzer graph from winamp.          *
 *  param1 - widthxheight of graph                       *
 *  param2 - options                                     *
 *	eg: $dll(winamp,1,hxw,0)                             *
 *	    $dll(winamp,1,hxw,u#1#FLW#100)                   *
 *********************************************************/
extern "C" WINAMP_API  char * 
__stdcall  function1(const char *param1, const char *param2)
{ 
	static string outbuf;
	static string savedParam1, savedParam2;

	try
	{
		if (!sharedMem)
			throw "[winamp: not running]";

		
		if ((strcmp(param1, savedParam1.c_str()) != 0) 
				|| (strcmp(param2, savedParam2.c_str()) != 0))
		{
			parseParameters(param1, param2);
			savedParam1 = param1;
			savedParam2 = param2;
		}

		if (currGraph)
		{
			if (applyNonLinear)
				renderNonLinearSA();
			else
				renderLinearSA();					
			currGraph->SetData(width, spectrumValues);
			currGraph->BarGraph(height, outbuf);
		}
		else
			outbuf = "";
		 
	} catch (char *e) {
		outbuf = e;
	}

	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 2                                    *
 * Draws 2nd line of a spectrum analyzer graph from winamp.*
 *  param1 - 0                                           *
 *  param2 - 0                                           *
 *	eg: $dll(winamp,2,0,0)                               *
 *********************************************************/
extern "C" WINAMP_API  char * 
__stdcall  function2(const char *param1, const char *param2)
{ 
	static string outbuf;

	try
	{
		if (!sharedMem)
			throw "[winamp: not running]";

		if (currGraph)
		{	
			if (2 > height)
				throw "no such line";
			currGraph->BarGraph(height-1, outbuf);
		}
		else
			outbuf = "no graph";
				 
	} catch (char *e) {
		outbuf = e;
	}

	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 3                                    *
 * Draws 3rd line of a spectrum analyzer graph from winamp.*
 *  param1 - 0                                           *
 *  param2 - 0                                           *
 *	eg: $dll(winamp,3,0,0)                               *
 *********************************************************/
extern "C" WINAMP_API  char * 
__stdcall  function3(const char *param1, const char *param2)
{ 
	static string outbuf;

	try
	{
		if (!sharedMem)
			throw "[winamp: not running]";

		if (currGraph)
		{	
			if (3 > height)
				throw "no such line";
			currGraph->BarGraph(height-2, outbuf);
		}
		else
			outbuf = "no graph";
				 
	} catch (char *e) {
		outbuf = e;
	}

	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 4                                    *
 * Draws 4th line of a spectrum analyzer graph from winamp.*
 *  param1 - 0                                           *
 *  param2 - 0                                           *
 *	eg: $dll(winamp,4,0,0)                               *
 *********************************************************/
extern "C" WINAMP_API  char * 
__stdcall  function4(const char *param1, const char *param2)
{ 
	static string outbuf;

	try
	{
		if (!sharedMem)
			throw "[winamp: not running]";

		if (currGraph)
		{		
			if (4 > height)
				throw "no such line";
			currGraph->BarGraph(height - 3, outbuf);
		}
		else
			outbuf = "no graph";
				 
	} catch (char *e) {
		outbuf = e;
	}

	return const_cast<char *>(outbuf.c_str());
}


/*********************************************************
 *         Function 2                                    *
 * Draws big numbers.                                    *
 *  param1 - [linenum#maxlines[#font]]                   *
 *  param2 - text to make big.                           *
 *	eg: $dll(winamp,1,hxw,line)                          *
 *********************************************************/
namespace { void parseParameters(const char *param1, const char *param2)
{ 

	
        // ====== Parse parameters ===========
		// param1 is options and takes the form:
		//   HeightxWidth#type#Direction#BarStyle##max
		// where:
		//   HeightxWidth is the size of the bar graph
		//   type is l == linear, n == nonlinear
		//   Direction is one of: d, u, l, r   [for down/up/left/right]
		//   BarStyle selects the custom characters to use
	    //   max - used for scaling graph.
	    
		// handle HeightxWidth
		height = atoi(param1); 
		const char *x = strchr(param1, 'x');
        if (x == NULL)
           throw "[HEIGHTxWIDTH required]";
		width = atoi(x+1);
		if ((width < 1) || (width > 40))
			throw "[Width out of range]";
		if ((height < 1) || (height > 4))
			throw "[Height out of range]";

		// handle Direction
		Graph::Direction direction = Graph::UP;
		if (strlen(param2) > 0)
		{
			switch (*param2)
			{
			case 'l': direction = Graph::LEFT; break;
			case 'r': direction = Graph::RIGHT; break;
			case 'u': direction = Graph::UP; break;
			case 'd': direction = Graph::DOWN; break;
			case '0': /*ignore*/ break;
			default: throw "[Invalid direction]";
			}
		} 
		

		// handle BarStyle
		unsigned int barStyle;
		const char *hash = strchr(param2, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[perf: bad bar style]";
			barStyle = atoi(hash);
		}
		else
			barStyle = 1;

		
		// handle linear/nonlinear falloff/not
		bool noweight = false, weight = false;
		bool nolinear = false, linear = false;
		bool nofalloff = false, falloff = false;
		if (hash != NULL)
			hash = strchr(hash, '#');

		if (hash != NULL)
		{
			hash ++;
	
			while (*hash && (*hash != '#'))
			{
				switch (*hash)
				{
				case 'W': weight = true; break;
				case 'w': noweight = true; break;
				case 'L': linear = true; break;
				case 'l': nolinear = true; break;
				case 'F': falloff = true; break;
				case 'f': nofalloff = true; break;
				default: throw "[Invalid spectrum option]";
				}
				hash++;
			}

			if ((noweight && weight) ||
				(nolinear && linear) ||
				(nofalloff && falloff))
				throw "[winamp: both on and off option present]";
		} 

		applyAWeighting = !noweight; // default to 'W'
		applyFalloff = !nofalloff; // default to 'F'
		applyNonLinear = !linear; // default to 'l'
		
		
		// handle max
		unsigned int max;
		if (hash != NULL)
			hash = strchr(hash, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[winamp: bad max]";
			max = atoi(hash);
		}
		else
			max = 80;

		// handle fallSpeed
		unsigned int fallSpeed;
		if (hash != NULL)
			hash = strchr(hash, '#');
	
		if (hash != NULL)
		{
			hash ++;
			if (!JustDigits(const_cast<char*>(hash)))
				throw "[winamp: bad fallspeed]";
			fallSpeed = atoi(hash);
		}
		else
			fallSpeed = 200;
		
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
			throw "[winamp: unknown bar style]";
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

		
		if (currGraph != 0)
			delete currGraph;
		currGraph = 0;

		dFalloffValue = static_cast<double>(fallSpeed) / 10.0;
		currGraph = new Graph(height,width,direction, barStyle, 0, max);
		spectrumValues.resize(width);
}
}

double CalcAWeighting(double freq)
{
    double rawWeighting=(pow(12200.0,2.0) *pow(freq,4.0)) 
		/ (((pow(freq,2.0)+ pow(20.6,2.0)) 
			* (pow(freq,2.0) + pow(12200.0,2.0))
			* pow((pow(freq,2.0) +pow(107.7,2.0)), 0.5)
			* pow((pow(freq,2.0) + pow(737.9,2.0)),0.5)));
    double dBA = 20.0 * log10(rawWeighting/0.794); // compare with 1kHz
	double percent = pow(2.0, dBA / 10.0); // convert to percentage

	return percent;
}

/******************************************************************************
Function : renderLinearSA
Purpose  : Renders the linear spectrum analyser
Parameters : pointer to visualisation module
Returns : 0 if successful, 1 if visualization should end.
Author  : Frank Verhamme    
******************************************************************************/
namespace { void renderLinearSA()
{
	static unsigned int analyzer[50];
	static unsigned int prevWidth = 0;
	static bool prevWeighting = false;
	static double logTemp[50][256];

	if ((prevWidth != width) || (prevWeighting != applyAWeighting))
	{
		prevWidth = width;
		prevWeighting = applyAWeighting;

		double step = 577.0 / (width+1); 
		for (unsigned int x = 0; x <= width; x++)
		{
			analyzer[x] = step*x;
		
			for (unsigned int i = 0; i <= 255; i++)
			{
				if (applyAWeighting)
				{
					double midFreq = (22000.0/576.0) * (analyzer[x] + step/2.0);
					logTemp[x][i] = static_cast<double>(i) * CalcAWeighting(midFreq);
				}
				else
					logTemp[x][i] = i;
			}
		}
	}

	
	int amo;

	
	unsigned int left_ch, right_ch;
	double temp;

	unsigned int x;
	unsigned int i;
	unsigned int itemp;
	for (x = 0; x < width; x++)  // columns
	{
		// get data for left and right channel
		right_ch = left_ch = 0;

		for (i=analyzer[x]; i < analyzer[x+1]; ++i)
		{
			if (i >= 576) throw "[i>576]";
			if (sharedMem->data[0][i] > left_ch)
				left_ch = (sharedMem->data[0][i]);
			if (sharedMem->data[1][i] > right_ch)
				right_ch = (sharedMem->data[1][i]);
		}

		// mix them together...
		itemp =  max(left_ch, right_ch);
		temp = logTemp[x][itemp];
		amo = AdjustFalloff(x, temp); 

		spectrumValues[x]=amo;
	}
}
}



/******************************************************************************
Function : renderNonLinearSA
Purpose  : Renders the non linear spectrum analyser
Parameters : pointer to visualisation module
Returns : 0 if successful, 1 if visualization should end.
Author  : JMattila    
******************************************************************************/
namespace { void renderNonLinearSA()
{
	int amo;
	double left_ch,right_ch;
	static unsigned int analyzer[50];
	static double logTemp[50][256];
	static unsigned int prevWidth = 0;
	static bool prevWeighting = false;
/*	Tested SPECTRUM ANALYZER data:  

	01 <-90hz = 1
	02 120hz = 3
	03 185hz = 5
	04 250hz = 7
	05 375hz = 9
	06 500hz = 13
	07 750hz = 19
	08 1000hz = 25
	09 1500hz = 38
	10 2250hz = 58 
	11 3000hz = 78
	12 4500hz = 116
	13 6000hz = 156
	14 7500hz = 195
	15 9000hz = 235
	16 10500hz = 274
	17 12000hz = 313
	18 13500hz = 352
	19 15000hz = 391
	20 16500hz = 430
*/
	if ((prevWidth != width) || (prevWeighting != applyAWeighting))
	{
		prevWidth = width;
		prevWeighting = applyAWeighting;

		for (unsigned int x=0; x < width; x++)
		{
			if (x == 0)
				analyzer[0] = 0;
			else
			{
				analyzer[x] = pow(10.0, static_cast<double>(x)*(log10(576.0))/width); //= analyzer[x*20/width];
				if (analyzer[x] <= analyzer[x-1])
					analyzer[x] = analyzer[x-1]+1;
				if (analyzer[x] > 576)
					analyzer[x] = 576;
			}			
		}
		analyzer[width] = 576;

		for (unsigned int x=0; x < width; x++)
		{
			
			for (unsigned int i = 0; i <= 255; i++)
			{
				if (applyAWeighting)
				{
					double midFreq = (22000.0/576.0) * (analyzer[x] + analyzer[x+1])/2.0;
					logTemp[x][i] = static_cast<double>(i) * CalcAWeighting(midFreq);
				}
				else
					logTemp[x][i] = i;
			}
		}
	}

    // spectrum analyser
	unsigned int i;
	unsigned int x;
	unsigned int iTemp;
	for (x = 0; x < width; x++)
	{
		left_ch = 0;
		right_ch = 0;
		
		for (i=analyzer[x]; i < analyzer[x+1]; ++i)
		{
			if (i >= 576) throw "[i>576]";
			if (sharedMem->data[0][i] > left_ch)
				left_ch = (sharedMem->data[0][i]);
			if (sharedMem->data[1][i] > right_ch)
				right_ch = (sharedMem->data[1][i]);
		}
		
		iTemp = max(left_ch, right_ch);
		amo = AdjustFalloff(x, logTemp[x][iTemp] ); //*1.1; );
		if (amo > 128 || amo < 0)
			iTemp = 0;
		spectrumValues[x]=amo;
	}
}


/******************************************************************************
Function : AdjustFalloff
Purpose  : Spectrum Analyser Falloff Code  
Parameters : x     : column position
             value : spectrum analyser value at position x
Returns : adjusted value as integer
Author  : Zlatko Novak / Markus Zehnder
******************************************************************************/
int AdjustFalloff(int x, double value)
{
	double adjusted;

	if (!applyFalloff) {
		return (int)value;
	}

	if (x >= 48)
		throw "[x>48]";

	if (darrFalloff[x] - dFalloffValue > value)
	{
		adjusted = darrFalloff[x] - dFalloffValue;
	} else {
		adjusted = value;
	}

	
	//Check to see if the previous falloff was huge. - James Maher 2002/01/15
	//if so cut off the huge values. (bars seem stuck on max otherwise) 
	//(takes a long time to change back to <= 16)
	// Added by James Maher
	if (darrFalloff[x] > 80)
	{ 
		darrFalloff[x] = 80; 
	} else { 
		//isn't hugely out of range, update to correct value
		darrFalloff[x] = adjusted;
	}
	return (int)adjusted;
}

}


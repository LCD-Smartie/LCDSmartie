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
#include <memory>
#include <vector>
#include <iterator>
#include <string>
#include "cpuload.h"
#include "graph.h"
#include "fonts.h"

using namespace std;



Graph::Graph(unsigned int height, unsigned int width, unsigned int time,
			 string& counterName, Direction direction, unsigned int barStyle, unsigned int minVal,
			 unsigned int maxVal)
			 : cpuload( new CpuLoad(((direction==UP) || (direction == DOWN))?width:height, time, counterName) ),
    width(width), height(height), sampleTime(time), direction(direction), barStyle(barStyle & STYLEMASK),
	barFlags(barStyle & ~STYLEMASK), minVal(minVal), maxVal(maxVal)
{
	if (barFlags & SIXWIDE)
		cellWidth = 6;
	else
		cellWidth = 5;

	if (barFlags & SEVENHIGH)
		cellHeight = 7;
	else
		cellHeight = 8;
};

Graph::~Graph()
{
}

unsigned int
Graph::GetCurrentValue()
{
	return cpuload->GetCpuUsage();
}

void
Graph::BarGraph(unsigned int lineNum, string& outbuf)
{
    unsigned int size = cpuload->GetSize();

	if (lineNum > height)
		throw string("[lineNum too high]");
	if (lineNum < 1)
		throw string("[lineNum too low]");
    // ====== Fetch line ===========
    
    vector<unsigned int> values;
    cpuload->GetCpuUsages(size, values);
    
    // scale values
    for (unsigned int count=0; count < size; count ++)
	{
		if (values[count] > maxVal) values[count] = maxVal;
		else if (values[count] < minVal) values[count] = minVal;

		if ((direction == UP) || (direction == DOWN))
			values[count] = ((values[count]-minVal) * (height*cellHeight)) / (maxVal - minVal);
		else
			values[count] = ((values[count]-minVal) * (width*cellWidth)) / (maxVal - minVal);
	}
        

	if (barFlags & TINY)
	{ 
		if ((direction == LEFT) || (direction == RIGHT))
			throw string("[Tiny: L/R direction not supported]");
		GetTinyGraph(outbuf, values);
	}
	else
	{
		if (direction == UP)
			GetLineVert(lineNum, values);
		else if	(direction == DOWN) // invert lines for DOWN
			GetLineVert(height-lineNum+1, values);
		else if (direction == RIGHT)
			GetLineHoz(lineNum, values);
		else if	(direction == LEFT)
			GetLineHoz(lineNum, values);

		// ====== Map onto character set ===========

		if (lineNum == height)
		{
			if (sizeof(fonts)/sizeof(fonts[0]) > barStyle)
				outbuf = fonts[barStyle][direction]; 
			else
				throw string("[perf: no such barstyle]");
		}
		else
			outbuf = "";
	}

	if (barFlags & TINY)
		assert(barStyle == 0);
   
	if (sizeof(fontmap)/sizeof(fontmap[0]) <= barStyle)
		throw string("[perf: bad font map]");

	vector<unsigned int>::iterator p;
	for (p=values.begin(); p != values.end(); p++)
	     outbuf += fontmap[barStyle][ *p ]; 
}
    


void
Graph::GetLineVert(unsigned int lineNum, vector<unsigned int>& values )
{  
    vector<unsigned int>::iterator p;   
    for (p=values.begin(); p != values.end(); p++)
    {
        if (*p > lineNum*cellHeight)
            *p = 9;
        else if ((*p) == (lineNum*cellHeight))
            *p = 8;
        else if (*p <= (lineNum-1)*cellHeight)
            *p = 0;
        else
            *p %= cellHeight;
    }    
}

void
Graph::GetLineHoz(unsigned int lineNum, vector<unsigned int>& values )
{  
    unsigned int bar = values[lineNum-1];   
	unsigned int c;

	values.resize(width);

	for (unsigned int i=0; i<width; i++)
    {
        if (bar > cellWidth*(i+1))
            c = 9;
        else if (bar == cellWidth*(i+1))
            c = 8;
        else if (bar <= cellWidth*i)
            c = 0;
        else
            c = bar % cellWidth;

		if (direction == RIGHT)
			values[i]=c;
		else
			values[width-(i+1)]=c;
    }    
}

void
Graph::GetTinyGraph(string& output, vector<unsigned int>& values)
{
	output = "";
	// we are going to define custom chars so that each value has a 1 pixel wide bar

	for (unsigned int i=0; i<width/cellWidth; i++)
	{
		char numstr[100];
		itoa(i+1, numstr, 10);
		output += string("$CustomChar(") + string(numstr);
		for (unsigned int j=1; j <= FullCellHeight; j++)
		{
			unsigned int value = 0;

			if (j <= cellHeight)
			{

				for (unsigned int k=1; k <= cellWidth; k++)
				{
					if (direction == DOWN) 
						value |= (((values[i*cellWidth + (k-1)] < j) ? 0 : 1) << (cellWidth-k));
					else
						value |= (((values[i*cellWidth + (k-1)] > (cellHeight-j)) ? 1 : 0) << (cellWidth-k));
				}
			}
			
			itoa(value, numstr, 10);
			output += string(",") + string(numstr);
		}
		output += ")";
	}


	values.resize(width/cellWidth);
	// these values will be mapped on to the custom chars after we return.
	for (unsigned int i=0; i<width/cellWidth; i++)
		values[i] = i+1;
}


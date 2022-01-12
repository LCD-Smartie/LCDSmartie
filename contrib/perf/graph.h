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
#ifndef G__GRAPH_H__
#define G__GRAPH_H__

#include <memory>
#include <vector>
#include <string>
#include <boost\shared_ptr.hpp>
#include "cpuload.h"

const unsigned int FullCellHeight = 8;
const unsigned int TINY = 128;
const unsigned int SIXWIDE = 64;
const unsigned int SEVENHIGH = 16;     // want bottom row empty
const unsigned int STYLEMASK = 7;

class Graph
{
	  public:
			 enum Direction {UP, DOWN, LEFT, RIGHT};
             Graph(unsigned int height, unsigned int width, unsigned int time,
				 std::string& counterName, Direction direction, unsigned int barStyle,
				 unsigned int min, unsigned int max);
             ~Graph();

			 void BarGraph(unsigned int lineNum, std::string& outbuf);
			 unsigned int GetHeight() { return height; };
			 unsigned int GetCurrentValue();
             
      private:
		  unsigned int cellWidth;
		  unsigned int cellHeight;
		  boost::shared_ptr<CpuLoad> cpuload;
		  unsigned int minVal, maxVal;
          unsigned int width;
          unsigned int height;
          unsigned int sampleTime;
		  unsigned int barStyle;
		  unsigned int barFlags;
		  Direction direction;  
		  void Graph::GetTinyGraph(std::string& output, std::vector<unsigned int>& values);
		  void GetLineVert(unsigned int lineNum, std::vector<unsigned int>& values );
		  void GetLineHoz(unsigned int lineNum, std::vector<unsigned int>& values );
};

#endif // G__GRAPH_H__

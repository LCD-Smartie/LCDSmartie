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
#include "graph.h"

const unsigned char CC1=176;
const unsigned char CC2=158;
const unsigned char CC3=131;
const unsigned char CC4=132;
const unsigned char CC5=133;
const unsigned char CC6=134;
const unsigned char CC7=135;
const unsigned char CC8=136;
const unsigned char SPACE=32;
const unsigned char NOTUSED='#'; // make visible just in case it's displayed in error.

// Fonts:

// fontmap[][8] is mid full bar (rather than full top bar)
// fontmap[][7] is top full bar
const unsigned char fontmap[][FullCellHeight+2] = {
		// {'0', '1', '2', '3', '4', '5', '6', '7', '8','9'}, // for debugging        
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, CC7, CC8, CC8}, // used for none and tiny graphs
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, CC7, CC8, CC8}, // font1
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, CC7, CC8, CC8}, // font2
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, NOTUSED, CC8, CC8}, // font3
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, NOTUSED, CC8, CC8}, // font4
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, CC7, CC8, CC8}, // font5
		{SPACE, CC1, CC2, CC3, CC4, CC5, CC6, NOTUSED, CC7, CC8}, // font6
	};


    
const char *(fonts[][4])={
		// font 0 - none; user makes their own
		{	"", "", "", ""	},
		// font 1 - solid
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,0,31)"  
         "$CustomChar(2,0,0,0,0,0,0,31,31)"
         "$CustomChar(3,0,0,0,0,0,31,31,31)"
         "$CustomChar(4,0,0,0,0,31,31,31,31)"
         "$CustomChar(5,0,0,0,31,31,31,31,31)"
         "$CustomChar(6,0,0,31,31,31,31,31,31)"
         "$CustomChar(7,0,31,31,31,31,31,31,31)"
         "$CustomChar(8,31,31,31,31,31,31,31,31)",
		//DOWN
		 "$CustomChar(1,31,0,0,0,0,0,0,0)"  
         "$CustomChar(2,31,31,0,0,0,0,0,0)"
         "$CustomChar(3,31,31,31,0,0,0,0,0)"
         "$CustomChar(4,31,31,31,31,0,0,0,0)"
         "$CustomChar(5,31,31,31,31,31,0,0,0)"
         "$CustomChar(6,31,31,31,31,31,31,0,0)"
         "$CustomChar(7,31,31,31,31,31,31,31,0)"
         "$CustomChar(8,31,31,31,31,31,31,31,31)",
		 //LEFT
		 "$CustomChar(1,1,1,1,1,1,1,1,1)"  
         "$CustomChar(2,3,3,3,3,3,3,3,3)"
         "$CustomChar(3,7,7,7,7,7,7,7,7)"
         "$CustomChar(4,15,15,15,15,15,15,15,15)"
         "$CustomChar(8,31,31,31,31,31,31,31,31)",
		 // RIGHT
		 "$CustomChar(1,16,16,16,16,16,16,16,16)"  
         "$CustomChar(2,24,24,24,24,24,24,24,24)"
         "$CustomChar(3,28,28,28,28,28,28,28,28)"
         "$CustomChar(4,30,30,30,30,30,30,30,30)"
         "$CustomChar(8,31,31,31,31,31,31,31,31)"
		},
		// font 2 - solid but 6 pixels wide (for CF displays)
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,0,63)"  
         "$CustomChar(2,0,0,0,0,0,0,63,63)"
         "$CustomChar(3,0,0,0,0,0,63,63,63)"
         "$CustomChar(4,0,0,0,0,63,63,63,63)"
         "$CustomChar(5,0,0,0,63,63,63,63,63)"
         "$CustomChar(6,0,0,63,63,63,63,63,63)"
         "$CustomChar(7,0,63,63,63,63,63,63,63)"
         "$CustomChar(8,63,63,63,63,63,63,63,63)",
		//DOWN
		 "$CustomChar(1,63,0,0,0,0,0,0,0)"  
         "$CustomChar(2,63,63,0,0,0,0,0,0)"
         "$CustomChar(3,63,63,63,0,0,0,0,0)"
         "$CustomChar(4,63,63,63,63,0,0,0,0)"
         "$CustomChar(5,63,63,63,63,63,0,0,0)"
         "$CustomChar(6,63,63,63,63,63,63,0,0)"
         "$CustomChar(7,63,63,63,63,63,63,63,0)"
         "$CustomChar(8,63,63,63,63,63,63,63,63)",
		 //LEFT
		 "$CustomChar(1,1,1,1,1,1,1,1,1)"  
         "$CustomChar(2,3,3,3,3,3,3,3,3)"
         "$CustomChar(3,7,7,7,7,7,7,7,7)"
         "$CustomChar(4,15,15,15,15,15,15,15,15)"
         "$CustomChar(5,31,31,31,31,31,31,31,31)"
		 "$CustomChar(8,63,63,63,63,63,63,63,63)",
		 // RIGHT
		 "$CustomChar(1,32,32,32,32,32,32,32,32)"  
         "$CustomChar(2,48,48,48,48,48,48,48,48)"
         "$CustomChar(3,56,56,56,56,56,56,56,56)"
         "$CustomChar(4,60,60,60,60,60,60,60,60)"
		 "$CustomChar(5,62,62,62,62,62,62,62,62)"
         "$CustomChar(8,63,63,63,63,63,63,63,63)"
		},
		// font 3 - solid but 6 pixels wide and only 7 high (for CF displays)
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,63,0)"  
         "$CustomChar(2,0,0,0,0,0,63,63,0)"
         "$CustomChar(3,0,0,0,0,63,63,63,0)"
         "$CustomChar(4,0,0,0,63,63,63,63,0)"
         "$CustomChar(5,0,0,63,63,63,63,63,0)"
         "$CustomChar(6,0,63,63,63,63,63,63,0)"
         "$CustomChar(8,63,63,63,63,63,63,63,0)",
		//DOWN
		 "$CustomChar(1,63,0,0,0,0,0,0,0)"  
         "$CustomChar(2,63,63,0,0,0,0,0,0)"
         "$CustomChar(3,63,63,63,0,0,0,0,0)"
         "$CustomChar(4,63,63,63,63,0,0,0,0)"
         "$CustomChar(5,63,63,63,63,63,0,0,0)"
         "$CustomChar(6,63,63,63,63,63,63,0,0)"
         "$CustomChar(8,63,63,63,63,63,63,63,0)",
		 //LEFT
		 "$CustomChar(1,1,1,1,1,1,1,1,0)"  
         "$CustomChar(2,3,3,3,3,3,3,3,0)"
         "$CustomChar(3,7,7,7,7,7,7,7,0)"
         "$CustomChar(4,15,15,15,15,15,15,15,0)"
         "$CustomChar(5,31,31,31,31,31,31,31,0)"
		 "$CustomChar(8,63,63,63,63,63,63,63,0)",
		 // RIGHT
		 "$CustomChar(1,32,32,32,32,32,32,32,0)"  
         "$CustomChar(2,48,48,48,48,48,48,48,0)"
         "$CustomChar(3,56,56,56,56,56,56,56,0)"
         "$CustomChar(4,60,60,60,60,60,60,60,0)"
		 "$CustomChar(5,62,62,62,62,62,62,62,0)"
         "$CustomChar(8,63,63,63,63,63,63,63,0)"
		},
		// font 4 - solid but only 7 high (for cheap displays)
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,31,0)"  
         "$CustomChar(2,0,0,0,0,0,31,31,0)"
         "$CustomChar(3,0,0,0,0,31,31,31,0)"
         "$CustomChar(4,0,0,0,31,31,31,31,0)"
         "$CustomChar(5,0,0,31,31,31,31,31,0)"
         "$CustomChar(6,0,31,31,31,31,31,31,0)"
         "$CustomChar(8,31,31,31,31,31,31,31,0)",
		//DOWN
		 "$CustomChar(1,31,0,0,0,0,0,0,0)"  
         "$CustomChar(2,31,31,0,0,0,0,0,0)"
         "$CustomChar(3,31,31,31,0,0,0,0,0)"
         "$CustomChar(4,31,31,31,31,0,0,0,0)"
         "$CustomChar(5,31,31,31,31,31,0,0,0)"
         "$CustomChar(6,31,31,31,31,31,31,0,0)"
         "$CustomChar(8,31,31,31,31,31,31,31,0)",
		 //LEFT
		 "$CustomChar(1,1,1,1,1,1,1,1,0)"  
         "$CustomChar(2,3,3,3,3,3,3,3,0)"
         "$CustomChar(3,7,7,7,7,7,7,7,0)"
         "$CustomChar(4,15,15,15,15,15,15,15,0)"
         "$CustomChar(8,31,31,31,31,31,31,31,0)",
		 // RIGHT
		 "$CustomChar(1,16,16,16,16,16,16,16,0)"  
         "$CustomChar(2,24,24,24,24,24,24,24,0)"
         "$CustomChar(3,28,28,28,28,28,28,28,0)"
         "$CustomChar(4,30,30,30,30,30,30,30,0)"
         "$CustomChar(8,31,31,31,31,31,31,31,0)"
		},
		// font 5 - thin bars
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,0,14)"  
         "$CustomChar(2,0,0,0,0,0,0,14,14)"
         "$CustomChar(3,0,0,0,0,0,14,14,14)"
         "$CustomChar(4,0,0,0,0,14,14,14,14)"
         "$CustomChar(5,0,0,0,14,14,14,14,14)"
         "$CustomChar(6,0,0,14,14,14,14,14,14)"
         "$CustomChar(7,0,14,14,14,14,14,14,14)"
         "$CustomChar(8,14,14,14,14,14,14,14,14)",
		//DOWN
		 "$CustomChar(1,14,0,0,0,0,0,0,0)"  
         "$CustomChar(2,14,14,0,0,0,0,0,0)"
         "$CustomChar(3,14,14,14,0,0,0,0,0)"
         "$CustomChar(4,14,14,14,14,0,0,0,0)"
         "$CustomChar(5,14,14,14,14,14,0,0,0)"
         "$CustomChar(6,14,14,14,14,14,14,0,0)"
         "$CustomChar(7,14,14,14,14,14,14,14,0)"
         "$CustomChar(8,14,14,14,14,14,14,14,14)",
		 //LEFT
		 "$CustomChar(1,0,0,1,1,1,1,0,0)"  
         "$CustomChar(2,0,0,3,3,3,3,0,0)"
         "$CustomChar(3,0,0,7,7,7,7,0,0)"
         "$CustomChar(4,0,0,15,15,15,15,0,0)"
         "$CustomChar(8,0,0,31,31,31,31,0,0)",
		 // RIGHT
		 "$CustomChar(1,0,0,16,16,16,16,0,0)"  
         "$CustomChar(2,0,0,24,24,24,24,0,0)"
         "$CustomChar(3,0,0,28,28,28,28,0,0)"
         "$CustomChar(4,0,0,30,30,30,30,0,0)"
         "$CustomChar(8,0,0,31,31,31,31,0,0)"
		},
		// font 6 - outlined bars [only 8 data points 0-7]
		{//UP
		 "$CustomChar(1,0,0,0,0,0,0,0,31)"  
         "$CustomChar(2,0,0,0,0,0,0,31,17)"
         "$CustomChar(3,0,0,0,0,0,31,17,17)"
         "$CustomChar(4,0,0,0,0,31,17,17,17)"
         "$CustomChar(5,0,0,31,17,17,17,17,17)"  // skip one position 
         "$CustomChar(6,0,31,17,17,17,17,17,17)"
		 "$CustomChar(7,31,17,17,17,17,17,17,17)" // top of bar
         "$CustomChar(8,17,17,17,17,17,17,17,17)",
		//DOWN
		 "$CustomChar(1,31,0,0,0,0,0,0,0)"  
         "$CustomChar(2,17,31,0,0,0,0,0,0)"
         "$CustomChar(3,17,17,31,0,0,0,0,0)"
         "$CustomChar(4,17,17,17,31,0,0,0,0)"
         "$CustomChar(5,17,17,17,17,17,31,0,0)"
         "$CustomChar(6,17,17,17,17,17,17,31,0)"
         "$CustomChar(7,17,17,17,17,17,17,17,31)"
         "$CustomChar(8,17,17,17,17,17,17,17,17)",
		 //LEFT
		 "$CustomChar(1,1,1,1,1,1,1,1,1)"  
         "$CustomChar(2,3,2,2,2,2,2,2,3)"
         "$CustomChar(3,7,4,4,4,4,4,4,7)"
         "$CustomChar(4,15,8,8,8,8,8,8,15)"
		 "$CustomChar(7,31,16,16,16,16,16,16,31)"
         "$CustomChar(8,31,0,0,0,0,0,0,31)",
		 // RIGHT
		 "$CustomChar(1,16,16,16,16,16,16,16,16)"  
         "$CustomChar(2,24,8,8,8,8,8,8,24)"
         "$CustomChar(3,28,4,4,4,4,4,4,28)"
         "$CustomChar(4,30,2,2,2,2,2,2,30)"
		 "$CustomChar(7,31,1,1,1,1,1,1,31)"
         "$CustomChar(8,31,0,0,0,0,0,0,31)"
		},
		
	};

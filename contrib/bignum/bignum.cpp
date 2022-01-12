/////////////////////////////////////////////////////////////////////////////
//
//  This file is part of the LCDSmartie BigNum Plugin.
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
#include "bignum.h"
#include <string>

using namespace std;

void
doDoubleHeight(unsigned int lineNum, unsigned int font, const char *text, string& outbuf)
{
	if (lineNum == 1)
	{
		// output font definition with the first line.
		if (font == 0)
			outbuf = ""; // let them make their own font
		else if (font == 1) // standard
			outbuf = "$CustomChar(1,1,1,1,1,1,1,1,1)"			// |
					"$CustomChar(2,31,17,17,17,17,17,17,17)"	// n shape
					"$CustomChar(3,17,17,17,17,17,17,17,31)"    // u shape
					"$CustomChar(4,31,1,1,1,1,1,1,1)"			// 7 shape
					"$CustomChar(5,31,16,16,16,16,16,16,16)"	// reversed 7 shape
					"$CustomChar(6,31,16,16,16,16,16,16,31)"    // reversed L shape
					"$CustomChar(7,31,17,17,17,17,17,17,31)"    // square
					"$CustomChar(8,31,1,1,1,1,1,1,31)";			// reversed C shape
		else if (font == 2) // bold
			outbuf = "$CustomChar(1,3,3,3,3,3,3,3,3)"
					"$CustomChar(2,31,31,27,27,27,27,27,27)"
					"$CustomChar(3,27,27,27,27,27,27,31,31)"
					"$CustomChar(4,31,31,3,3,3,3,3,3)"
					"$CustomChar(5,31,31,24,24,24,24,24,24)"
					"$CustomChar(6,31,31,24,24,24,24,31,31)"
					"$CustomChar(7,31,31,27,27,27,27,31,31)"
					"$CustomChar(8,31,31,3,3,3,3,31,31)";
		else if (font == 3) // standard, but only use last row if full row needed.
			outbuf = "$CustomChar(1,1,1,1,1,1,1,1,0)"
					"$CustomChar(2,31,17,17,17,17,17,17,0)"
					"$CustomChar(3,17,17,17,17,17,17,17,31)"
					"$CustomChar(4,31,1,1,1,1,1,1,0)"
					"$CustomChar(5,31,16,16,16,16,16,16,0)"
					"$CustomChar(6,31,16,16,16,16,16,16,31)"
					"$CustomChar(7,31,17,17,17,17,17,17,31)"
					"$CustomChar(8,31,1,1,1,1,1,1,31)";
		else if (font == 4) // bold, but only use last row if full row needed.
			outbuf = "$CustomChar(1,3,3,3,3,3,3,3,0)"
					"$CustomChar(2,31,31,27,27,27,27,27,0)"
					"$CustomChar(3,27,27,27,27,27,27,31,31)"
					"$CustomChar(4,31,31,3,3,3,3,3,0)"
					"$CustomChar(5,31,31,24,24,24,24,24,0)"
					"$CustomChar(6,31,31,24,24,24,24,31,31)"
					"$CustomChar(7,31,31,27,27,27,27,31,31)"
					"$CustomChar(8,31,31,3,3,3,3,31,31)";
		else 
		{
			outbuf = "[bignum: no such font]";
			return;
		}
	}
	else
		outbuf = "";

	unsigned char fontmap[2][10] = {{158,176,132,132,131,133,133,132,135,135},
									{131,176,134,136,176,136,135,176,131,176}};

	for (unsigned int i=0; i < strlen(text); i++)
	{
		if ((text[i] >= '0') && (text[i] <='9'))
			outbuf += fontmap[lineNum-1][text[i]-'0'];
		else if (text[i] == ':')
			outbuf += '.';
		else if (lineNum == 2)
			outbuf += text[i];
		else 
			outbuf += ' ';
	}
}


void
doDoubleHeightWidth(unsigned int lineNum, unsigned int font, const char *text, string& outbuf)
{
	if (lineNum == 1)
	{
		// output font definition with the first line.
		if (font == 0)
			outbuf = ""; // let them make their own font
		else if (font == 1) // standard
			outbuf =  "$CustomChar(1,31,16,16,16,16,16,16,16)"  // reversed 7 shape
					  "$CustomChar(2,31,1,1,1,1,1,1,1)"         // 7 shape
					  "$CustomChar(3,16,16,16,16,16,16,16,31)"  // L shape
					  "$CustomChar(4,1,1,1,1,1,1,1,31)"         // reversed L shape
					  "$CustomChar(5,31,16,16,16,16,16,16,31)"  // C shape
					  "$CustomChar(6,31,1,1,1,1,1,1,31)"        // reversed C shape
					  "$CustomChar(7,0,0,0,0,0,0,0,31)"         // _
					  "$CustomChar(8,31,0,0,0,0,0,0,0)";        // -

    	else if (font == 2) // bold
			outbuf =  "$CustomChar(1,31,31,24,24,24,24,24,24)"
					  "$CustomChar(2,31,31,3,3,3,3,3,3)"
					  "$CustomChar(3,24,24,24,24,24,24,31,31)"
					  "$CustomChar(4,3,3,3,3,3,3,31,31)"
					  "$CustomChar(5,31,31,24,24,24,24,31,31)"
					  "$CustomChar(6,31,31,3,3,3,3,31,31)"
					  "$CustomChar(7,0,0,0,0,0,0,31,31)"
					  "$CustomChar(8,31,31,0,0,0,0,0,0)";
		else if (font == 3) // standard, but only use last row if full row needed.
			outbuf =  "$CustomChar(1,31,16,16,16,16,16,16,0)"  // reversed 7 shape
					  "$CustomChar(2,31,1,1,1,1,1,1,0)"         // 7 shape
					  "$CustomChar(3,16,16,16,16,16,16,16,31)"  // L shape
					  "$CustomChar(4,1,1,1,1,1,1,1,31)"         // reversed L shape
					  "$CustomChar(5,31,16,16,16,16,16,16,31)"  // C shape
					  "$CustomChar(6,31,1,1,1,1,1,1,31)"        // reversed C shape
					  "$CustomChar(7,0,0,0,0,0,0,0,31)"         // _
					  "$CustomChar(8,31,0,0,0,0,0,0,0)";        // -
		else if (font == 4) // bold, but only use last row if full row needed.
			outbuf =  "$CustomChar(1,31,31,24,24,24,24,24,0)"
					  "$CustomChar(2,31,31,3,3,3,3,3,0)"
					  "$CustomChar(3,24,24,24,24,24,24,31,31)"
					  "$CustomChar(4,3,3,3,3,3,3,31,31)"
					  "$CustomChar(5,31,31,24,24,24,24,31,31)"
					  "$CustomChar(6,31,31,3,3,3,3,31,31)"
					  "$CustomChar(7,0,0,0,0,0,0,31,31)"
					  "$CustomChar(8,31,31,0,0,0,0,0,0)";
		else 
		{
			outbuf = "[bignum: no such font]";
			return;
		}
	}
	else
		outbuf = "";

	const unsigned char CC1=176, CC2=158, CC3=131, CC4=132, CC5=133, CC6=134, CC7=135, CC8=136, SP=' ';

	unsigned char fontmap[10][2][2] = {
		/*0*/{
				{CC1,  CC2},
				{CC3,  CC4}
			},
		/*1*/{
				{CC2,   SP},
				{CC4,  CC7}
			},
		/*2*/{
				{CC8,  CC6},
				{CC5,  CC7}
			},
		/*3*/{
				{CC8,  CC6},
				{CC7,  CC4}
			},
		/*4*/{
				{CC3,  CC4},
				{ SP,  CC2}
			},
		/*5*/{
				{CC5,  CC8},
				{CC7,  CC6}
			},
		/*6*/{
				{CC1,  CC8},
				{CC5,  CC6}
			},
		/*7*/{
				{CC8,  CC2},
				{ SP,  CC2}
			},
		/*8*/{
				{CC5,  CC6},
				{CC3,  CC4}
			},
		/*9*/{
				{CC5,  CC6},
				{CC7,  CC4}
			}
	};

	for (unsigned int i=0; i < strlen(text); i++)
	{
		if ((text[i] >= '0') && (text[i] <='9'))
		{
			outbuf += fontmap[text[i]-'0'][lineNum-1][0];
			outbuf += fontmap[text[i]-'0'][lineNum-1][1];
		}
		else if (text[i] == ':')
			outbuf += '.';
		else if ((text[i] == '-') && (lineNum == 1)) 
			outbuf += CC7;
		else if ((lineNum == 2) && (text[i] != ':') && (text[i] != '-'))
			outbuf += text[i];
		else 
			outbuf += ' ';
	}
}


void
doQuadHeight(unsigned int lineNum,  unsigned int font, const char *text, string& outbuf)
{
	if (lineNum == 1)
	{
		// output font definition with the first line.
		if (font == 0)
			outbuf = ""; // let them make their own font
		else if (font == 1)
			outbuf = "$CustomChar(1,31,31,31,31,0,0,0,0)" // top half
					"$CustomChar(2,0,0,0,0,31,31,31,31)" // bot half
					"$CustomChar(3,31,31,31,31,31,31,31,31)" // full block
					"$CustomChar(4,3,3,3,3,3,3,3,3)" // right half
					"$CustomChar(5,31,30,30,28,28,24,24,16)" // tl corner
					"$CustomChar(6,1,3,3,7,7,15,15,31)" // br corner
					"$CustomChar(7,31,15,15,7,7,3,3,1)" // tr corner
					"$CustomChar(8,16,24,24,28,28,30,30,31)"; // bl corner
		else if (font == 2)
			outbuf = "$CustomChar(1,31,31,31,31,0,0,0,0)" // top half
					"$CustomChar(2,0,0,0,0,31,31,31,31)" // bot half
					"$CustomChar(3,31,31,31,31,31,31,31,31)" // full block
					"$CustomChar(4,3,3,3,3,3,3,3,0)" // right half
					"$CustomChar(5,31,30,30,28,28,24,24,0)" // tl corner
					"$CustomChar(6,1,3,3,7,7,15,15,31)" // br corner
					"$CustomChar(7,31,15,15,7,7,3,3,0)" // tr corner
					"$CustomChar(8,16,24,24,28,28,30,30,31)"; // bl corner
		else 
		{
			outbuf = "[bignum: no such font]";
			return;
		}
	}
	else
		outbuf = "";

	const unsigned char TH=176, BH=158, FB=131, RH=132, TLC=133, BRC=134, TRC=135, BLC=136, SP=' ';
	

	unsigned char fontmap[10][4][3] = {
		/*0*/{
				{BRC,  FB, BLC},
				{ FB,  SP,  FB},
				{ FB,  SP,  FB},
				{TRC,  FB, TLC}
			},
		/*1*/{
				{ SP,  FB, SP},
				{ SP,  FB, SP},
				{ SP,  FB, SP},
				{ SP,  FB, SP}
			},
		/*2*/{
				{BRC,  FB, BLC},
				{ SP, BRC, TLC},
				{BRC, TLC,  SP},
				{ FB,  FB,  FB}
			},
		/*3*/{
				{BRC,  FB, BLC},
				{ SP,  SP,  FB},
				{ SP,  TH,  FB},
				{TRC,  FB, TLC}
			},
		/*4*/{
				{ FB,  SP,  FB},
				{ FB,  FB,  FB},
				{ SP,  SP,  FB},
				{ SP,  SP,  FB}
			},
		/*5*/{
				{ FB,  FB,  FB},
				{ FB,  BH,  BH},
				{ SP,  SP,  FB},
				{ FB,  FB, TLC}
			},
		/*6*/{
				{BRC,  FB, BLC},
				{ FB,  SP,  SP},
				{ FB,  TH, BLC},
				{TRC,  FB, TLC}
			},
		/*7*/{
				{ FB,  FB,  FB},
				{ SP,  SP,  FB},
				{ SP,  SP,  FB},
				{ SP,  SP,  FB}
			},
		/*8*/{
				{BRC,  FB, BLC},
				{ FB,  SP,  FB},
				{ FB,  TH,  FB},
				{TRC,  FB, TLC}
			},
		/*9*/{
				{BRC,  FB, BLC},
				{TRC,  BH,  FB},
				{ SP,  SP,  FB},
				{ SP,  SP,  FB}
			}
	};

	for (unsigned int i=0; i < strlen(text); i++)
	{
		if ((text[i] >= '0') && (text[i] <='9'))
		{
			outbuf += fontmap[text[i]-'0'][lineNum-1][0];
			outbuf += fontmap[text[i]-'0'][lineNum-1][1];
			outbuf += fontmap[text[i]-'0'][lineNum-1][2];
		}
		else if ((text[i] == ':') && ((lineNum == 2) || (lineNum == 3)))
			outbuf += 'o';
		else if ((lineNum == 4) && (text[i] != ':'))
			outbuf += text[i];
		else 
			outbuf += ' ';
	}
}


/*********************************************************
 *         Function 1                                    *
 * Draws big numbers. (1x2 font)                         *
 *  param1 - [linenum[#font]]                            *
 *  param2 - text to make big.                           *
 *	eg: $dll(bignum,1,12:35)                             *
 *	    $dll(bignum,2,12:35)                             *
 *********************************************************/
extern "C" BIGNUM_API  char * 
__stdcall  function1(const char *param1, const char *param2)
{ 
	static string outbuf;

	unsigned int lineNum = atoi(param1);

	const char *hash = strchr(param1, '#');
	
    // handle font
	unsigned int font;
	if (hash)
		hash = strchr(hash, '#');
	
	if (hash != NULL)
	{
		hash ++;
		font = atoi(hash);
	}
	else
		font = 1;

	if ((lineNum < 1) || (lineNum > 2))
		return "[bignum: invalid line number]";

    doDoubleHeight(lineNum, font, param2, outbuf);
 
	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 2                                    *
 * Draws big numbers. (2x2 font)                         *
 *  param1 - [linenum[#font]]                            *
 *  param2 - text to make big.                           *
 *	eg: $dll(bignum,1,12:35)                             *
 *	    $dll(bignum,2,12:35)                             *
 *********************************************************/
extern "C" BIGNUM_API  char * 
__stdcall  function2(const char *param1, const char *param2)
{ 
	static string outbuf;

	unsigned int lineNum = atoi(param1);

	const char *hash = strchr(param1, '#');
	
    // handle font
	unsigned int font;
	if (hash)
		hash = strchr(hash, '#');
	
	if (hash != NULL)
	{
		hash ++;
		font = atoi(hash);
	}
	else
		font = 1;

	if ((lineNum < 1) || (lineNum > 2))
		return "[bignum: invalid line number]";

    doDoubleHeightWidth(lineNum, font, param2, outbuf);
 
	return const_cast<char *>(outbuf.c_str());
}

/*********************************************************
 *         Function 3                                    *
 * Draws big numbers.   (3x4 font)                       *
 *  param1 - [linenum[#font]]                            *
 *  param2 - text to make big.                           *
 *	eg: $dll(bignum,1,12:35)                             *
 *	    $dll(bignum,2,12:35)                             *
 *	    $dll(bignum,3,12:35)                             *
 *	    $dll(bignum,4,12:35)                             *
 *********************************************************/
extern "C" BIGNUM_API  char * 
__stdcall  function3(const char *param1, const char *param2)
{ 
	static string outbuf;

	unsigned int lineNum = atoi(param1);

	const char *hash = strchr(param1, '#');
	
    // handle font
	unsigned int font;
	if (hash)
		hash = strchr(hash, '#');
	
	if (hash != NULL)
	{
		hash ++;
		font = atoi(hash);
	}
	else
		font = 1;

	if ((lineNum < 1) || (lineNum > 4))
		return "[bignum: invalid line number]";

    doQuadHeight(lineNum, font, param2, outbuf);
 
	return const_cast<char *>(outbuf.c_str());
}


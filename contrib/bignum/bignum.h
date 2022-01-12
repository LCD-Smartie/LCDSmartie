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


#ifdef BIGNUM_EXPORTS
#define BIGNUM_API __declspec(dllexport)
#else
#define BIGNUM_API __declspec(dllimport)
#endif

// This class is exported from the bignum.dll
class BIGNUM_API Cbignum {
public:
	Cbignum(void);
	// TODO: add your methods here.
};

extern BIGNUM_API int nbignum;

BIGNUM_API int fnbignum(void);

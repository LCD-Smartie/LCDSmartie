/////////////////////////////////////////////////////////////////////////////
//
//  This file is part of the LCDSmartie Menu Plugin.
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
#include "menu.h"
#include "menus.h"
#include <string>
#include <vector>
#include <map>

using namespace std;

typedef map<string, Menu> TMenus;
TMenus menus;
string currentMenu;


extern "C" MENU_API  void
__stdcall  SmartieFini()
{
	menus.clear();
}


string GetActiveMenuLine(unsigned int line)
{
	TMenus::iterator active = menus.find(currentMenu);

	if (active == menus.end())
		return "[menu: no active menu]";

	return active->second.GetLine(line);
}

/*******************************************************************
 *         Function 1                                              *
 * Creates a menu, and displays first item.				           *
 *  param1 - [length]#[menuitem=action]#[menuitem2=action]         *
 *  param2 - name.                                                 *
 *	eg: $dll(menu,1,2#Test=GotoScreen(1)#Test2=GotoScreen(2),Menu1)*
 *******************************************************************/
extern "C" MENU_API  char * 
__stdcall  function1(const char *param1, const char *param2)
{ 
	static string outbuf;

	string menuName(param2);
	currentMenu = menuName;

	TMenus::iterator menu;

	menu = menus.find(menuName);

	if (menu != menus.end())
	{ // found menu
		string origDef = menu->second.GetOrigDef();

		if (origDef != string(param1))
		{ // definition has changed - recreate.
			menus.erase(menu);
			menu = menus.end();
		}			
	}

	if (menu == menus.end())
	{ // not found (or erased) - create it.

		unsigned int length = atoi(param1);

		if (length < 1)
			return "[menu: bad length]";

		vector<menuitem> items;
		const char *hash = strchr(param1, '#');
		while (hash && *hash)
		{
			hash++;

			menuitem item;

			const char *nexthash = strchr(hash, '#');
			if (!nexthash)
				nexthash = param1 + strlen(param1);

			const char *equals = strchr(hash, '=');

			if ((!equals) || (nexthash < equals))
				return "[menu: missing =]";

            item.name = string(hash, equals);
			item.action = string(equals+1, nexthash);
			items.push_back(item);

			hash = nexthash;
		}

		if (items.size() == 0)
			return "[menu: zero items?]";
	
		menus[menuName] = Menu(length, items, param1);
	}

	outbuf = GetActiveMenuLine(1);

	return const_cast<char *>(outbuf.c_str());
}

/*******************************************************************
 *         Function 2                                              *
 * Displays an item.	                    			           *
 *  param1 - line										           *
 *  param2 - ignored                                               *
 *	eg: $dll(menu,1,2,0)										   *
 *******************************************************************/
extern "C" MENU_API  char * 
__stdcall  function2(const char *param1, const char *param2)
{ 
	static string outbuf;

	outbuf = GetActiveMenuLine(atoi(param1));

	return const_cast<char *>(outbuf.c_str());
}

/*******************************************************************
 *         Function 7                                              *
 * Do action in param1 if in menu otherwise in param2 if not in menu*
 *******************************************************************/
extern "C" MENU_API  char * 
__stdcall  function7(const char *param1, const char *param2)
{ 
	static string outbuf;

	if (currentMenu == "")
		outbuf = param1;
	else
		outbuf = param2;
		
	return const_cast<char *>(outbuf.c_str());
}

/*******************************************************************
 *         Function 8                                              *
 * Changed Screen notification									   *
 *******************************************************************/
extern "C" MENU_API  char * 
__stdcall  function8(const char *param1, const char *param2)
{ 
	currentMenu = "";
	return "";
}

/*******************************************************************
 *         Function 9                                              *
 * Receives input and decides on action to take.		           *
 *  param1 - input										           *
 *  param2 - default action (if not in menu)                       *
 *	eg: $dll(menu,9,u,$GotoScreen(1))							   *
 *******************************************************************/
extern "C" MENU_API  char * 
__stdcall  function9(const char *param1, const char *param2)
{ 
	static string outbuf;

	TMenus::iterator active = menus.find(currentMenu);

	if (active == menus.end())
	{
		// active menu not found
		outbuf = param2;
	}
	else
	{
		Menu::Events event;
		switch (param1[0]) 
		{
		case 'n': event = Menu::NEXT; break;
		case 'p': event = Menu::PREV; break;
		case 's': event = Menu::SELECT; break;
		default:
			return "[menu: unknown event]";
		}

		outbuf = active->second.Event(event);
	}

	return const_cast<char *>(outbuf.c_str());
}

#ifndef G__MENUS_H__
#define G__MENUS_H__
#include <vector>
#include <string>

struct menuitem {
	std::string name;
	std::string action;
};


class Menu
{
public:
	enum Events {NEXT, PREV, SELECT};
	Menu(unsigned int displayed, std::vector<menuitem> items, std::string origDef);
	Menu() {};
	~Menu() { };
	std::string GetLine(unsigned int line);
	Menu& operator= (const Menu& rhs);
	std::string GetOrigDef() { return origDef; };
	std::string Event(Events event);
private:
	std::vector<menuitem> items;
	std::string origDef;
	unsigned int currentItem;
	unsigned int displayed;
	unsigned int windowStart;
	unsigned int windowPos;
};

Menu::Menu(unsigned int displayed, std::vector<menuitem> items, std::string origDef) 
	: displayed(displayed), origDef(origDef), items(items)
{
	currentItem = 0;
	windowStart = 0;
	windowPos = 0;
}

Menu& Menu::operator= (const Menu& rhs)
{
	if (&rhs != this)
	{
		items = rhs.items;
		currentItem = 0;
		origDef = rhs.origDef;
		displayed = rhs.displayed;
		windowPos = 0;
		windowStart = 0;
	}
	return *this;
}

std::string Menu::GetLine(unsigned int line)
{
	line --;
	if ((line+windowStart >= items.size()) || (line > displayed))
		return "[menu: no such line]";

	if (line+windowStart == currentItem)
		return std::string(">") + items[line+windowStart].name;
	else
		return std::string(" ") + items[line+windowStart].name;
}

std::string Menu::Event(Events event)
{
	if (event == SELECT)
	{
		if (currentItem < items.size())
		{
			std::string& action = items[currentItem].action;
			currentItem = 0;
			windowStart = 0;
			windowPos = 0;
			return action;
		}
		else
			return "[menu: no current item]";
	}
	else if (event == PREV)
	{
		if (currentItem > 0)
		{
			currentItem--;
			if (windowPos > 0)
				windowPos --;
			else
				windowStart--;
		}
	}
	else if (event == NEXT)
	{
		if (currentItem+1 < items.size())
		{
			currentItem++;
			if (windowPos+1 < displayed)
				windowPos++;
			else
				windowStart++;
		}
	}

	return "";
}

#endif // G__MENUS_H__
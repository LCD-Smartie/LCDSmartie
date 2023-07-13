# LCD Smartie
### From [wikipedia.org/wiki/LCD_Smartie](https://wikipedia.org/wiki/LCD_Smartie)
>LCD Smartie is open-source software for Microsoft Windows which allows a character LCD to be used as an auxiliary display device for a PC.
Supported devices include displays based on the Hitachi HD44780 Controller, the Matrix Orbital Serial/USB LCD, and Palm OS devices (when used in conjunction with PalmOrb).
The program has built in support for many systems statistics (i.e. cpu load, network utilization, free disk space...), downloading RSS feeds, Winamp integration and support for several other popular applications.
To support less common applications LCD Smartie uses a powerful plugin system.

### LCDSmartie can also be used as a desktop ticker even if you don't have an lcd display

### This project has been ported to Lazarus (https://www.lazarus-ide.org)
### It is a Work in Progress

### About This Repo
After having no updates for a little over 10 years and very little activity on the lcd smartie forums it seems that the original project has been abandoned by its developers.
I have therefore forked from the original project and have added some fixes and improvements.

### Issues
While there is much information on the lcd smartie forums, any issues with this fork should be reported here. It's high likely you wouldn't get a reply over there anyway.
- Only desktop and matrix display drivers have been ported as I don't own the hardware to test the others.
- Some contrib plugins have not been ported yet. One in particular uses WMCopyData and I think Lazarus doesn't support that
- There may/will be bugs

### Log and Information
- ### 5.5.4.21
- Issue #7 A couple of issues but seems to be fixed now. Fixed the DISPLAYDLL_CustomCharIndex dll function handler
- Issue #8 Is now fixed. Thanks to limbo666 and rdoursenaud for testing and confirming this.
- Issue #14 A bit of silliness in the way the client area was calculated. Fix by manually specifying paintbox size
- ### 5.5.4.15
- Issue #10 revert variable resolver behaviour and #12 Implement custom LCD size options
- Also implemented recursive variable resolver for some other variables. Far from complete as some other resolver functions need complete re-write to take advantage.
- Can now set custom screen sizes up to 8x100. Re-writing has allowed for even larger displays in future. Some plugins while will likely work it has to be remembered that they were written with only four lines in mind.
- ### 5.5.3.6
- LCDSmartie.exe.Manifest must be kept alongside the program to be able to detect versions of Windows higher than 8
- new functions: $SysCPUCoreUsage(CPU, Core) and $SysCPUCoreSpeed(CPU, Core) for % usage or MHz speed for a specific core. CPU is the physical core index. Core is the core index on the physical CPU. Example: $SysCPUCoreUsage(0,0) for physical CPU 0 core 0, $SysCPUCoreUsage(0,1) physical 0 core 1. You can use $SysCPUCoreUsage(_Total) for an overall total or $SysCPUCoreUsage(0,_Total) for the first physical CPU total.
- Changed variable resolver. Variables were being resolved on a top down basis so while you could nest some variables it would only work in a certain order eg: you could $Dll($Sys()) but not $Sys($Dll()). Now attempts to resolve variables until no more can be resolved. However, without a complete re-write you cannot $Dll($Dll()) but, you can "$Store($Dll(),0) $Dll($Fetch(0))"
- New functions: $Store(value, slot) to store a value. $Fetch(slot) to return a value from storage
- New functions: $Round(num, places) for rounding decimal numbers. $Add(num1, Num2) for adding two numbers. $Sub(num1, num2) for subtracting numbers. $Mul(num1, num2) for multiplication. $Div(num1, num2) for division
- New feature: perf counter tab
- I have shown the desktop.dll display some love. It now has a config file to save its position, transparency and stay on top. Options can be found by right clicking the display. It is now frameless and taskbar buttonless for minimum intrusion and can be dragged around simply by left click and draging on the display.
- Issue #7 Setup text fields not accepting foreign characters. Switched charset to Unicode from ANSI hoping this will fix the issue
- Issue #6 LCD resets on apply. Change to only reset if a driver or driver parameter is changed
- Issue #5 Incorrect cpu stats might be caused by using localized windows. Switch to using a language neutral function
- Issue #4 Third party LCD2USB display driver not working. Not sure what caused this but re-compiling and linking to libusb available in vcpkg solved the issue. Driver is GPL licensed so will include it in LCD Smartie source
- ### 5.5.3.4
- Fixed Issue where Windows 11 reports incorrect cpu usage
- New driver for EZIO serial display
- Added option to start as administrator and use task scheduler for autostart
- Removed SETI tab and replaced with BOINC
- Fixed issue where some actions had spaces in the names
- Major re-write of dnbridge.dll to compile under VS2022
- Added 64bit target but CPU detection code is not 64bit compatible so it has had to go. There are plugins which can provide the same info though
- Can now use variables in shutdown message
- Fixed broadcast message handler for shutdown and logoff message screen
- Reduced some CPU usage in virtual screen update and actions processing
- Fixed a couple of issues with startup hiding and always on top.
- Added a new project to have a stand alone config editor
- ported winampctrl to lazarus
- removed dependancy on cooltrayicon - lazarus has its own tray icon package
- Fixed half screen issue when switching lcd emulation mode
- Line edit is now a button as double clicking should select a word not open a window
- Main, setup and line edit window positions now saved in config
- Changed handling of multi byte characters - some characters were getting messed up. It is hard coded but more can be added if needed
- Re-designed actions tab - Easier editing and adding new actions
- Remote screen function - Network receive screen data from another smartie
- Folding@home stats working again - Added some new stats too
- Detect if a certain app is running - Useful for actions based on an app being active
- More screens - You can now have up to 99 - It was annoying getting an idea for another screen then realising that an old one had to go
- More Email accounts - Up to 99 can be monitored - once upon a time 10 email addresses were enough for me too
- Email SSL is now working - Tested with Gmail and Outlook
- Fixed the SSL code for HTTPS RSS feeds too - With the last official release, the only default feed still working was BBC News
- Custom character editor - It wasn't that difficult before but now it's even easier to create custom characters
- Stats for more network adapters - latest versions of windows install many virtual network adapters my windows 10 computer has over 40 but there was a limit of 10 adapters stats that could be used now it's 99. Also - 
- A Button to list the index of network adapters - beats going through them one by one to find the one you want
- Fetching local ip address has been fixed - Before it would just fetch the first one it found which was a problem if a virtual adapter with an address assigned has a lower index than the physical interface. Now it's per adapter
- Detect if screen saver is active - Can be used with actions to turn off the display or show another screen or theme
- Detect if a full screen app is running - Again as above can be used with actions to set another screen
- Detect if a full screen 3D game is running - maybe use actions to set a screen or theme showing temperatures
- Buttons to rearrange screens - Copy to/move to/swap with another screen
- Many more under the hood fixes

### Roadmap
As I'm only one man that does this in my spare time I probably will not get to work on it as much as I'd like but, I will try to bring fixes and updates as often as I can.

### Get LCDSmartie
download here - [latest](https://github.com/stokie-ant/lcdsmartie-laz/releases/latest)

### Building
see the file [BUILDING.txt](BUILDING.txt)


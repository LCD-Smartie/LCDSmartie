Notes on display DLLs.

The crystal, HD44780, and matrix drivers are simply ports from v5.3 into the DLL
display framework and are fully compatible with the previous version of LCD Smartie.
Please use the on screen help to set the parameters for these displays.

desktop.dll    - an on-screen emulator for LCD displays.  Supports backlighting and
                 custom character commands, and is supplied for testing primarily
                 custom character plugins and features used by LCD Smartie.  Although
                 the new "LCD Emulation" mode allows similar functionality, this
                 DLL supports large resizing, and more realistic character gaps.

futabavfd.dll  - driver for the Futaba VFD and LCD displays.  Modified from the HD44780
                 driver, this driver supports changing the brightness of the VFD.

imon.dll       - driver for iMON VFD displays.  Requires SG_VFD.DLL which is supplied
                 with the iMON package.  Note that this display does not support
                 custom characters, although the WinAmp spectrum analyzer should
                 function correctly.

imond.dll      - driver for iMON VFD displays.  Does not require the SG_VFD.DLL although
                 it does require the Windows driver for the iMON display to be installed.
                 This directwrite driver supports currently available iMON displays, and
                 should be faster and require less resources than using the SG_VFD.DLL
                 driver, but future compatibility is not guaranteed.  If stability issues
                 arise with this driver, consider switching to the imon.dll driver.

irtrans.dll    - driver for communicating to the VFD displays commonly found in OrigenAE/Dign
                 and Zalman HTPC cases.  You should install IRTrans per the instructions
                 supplied with the cases, and run the IRTrans server as this driver
                 communicates with the server, not directly to the display.

pertelian.dll  - driver for pertelian LCD and IEE VFD displays.  Roughly based on the
                 seetron driver, this driver is also untested and was written from specs,
                 so if you test this driver and find bugs or find it to function correctly,
                 please leave feedback on the LCD Smartie forum for the developers.

seetron.dll    - driver for both protocol versions of Seetron LCD and VFD screens.  This
                 driver is untested and was written from specs, so if you test this driver
                 and find bugs or find it to function correctly, please leave feedback on
                 the LCD Smartie forum for the developers.

testdriver.dll - driver to reincorporate the "test driver" capability that was present in
                 LCD Smartie v5.3 before the change to the DLL display driver model.  This
                 DLL uses the INI file testdriver.ini located in the LCDSmartie.exe directory
                 (not the displays DLL directory) to control the initialization, finalization
                 and line location commands, and allows you to remap as many characters
                 as you want to other characters.  Please see the website for more details.


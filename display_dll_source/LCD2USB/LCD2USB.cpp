/* LCD2USB.cpp : LCD-Smartie DLL Driver for Till Harbaum's LCD2USB-Interface
 *
 * Copyright (c) 2007 Harald Koerfgen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2
 * as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

//$Id: 

#include <windows.h>
#include <stdlib.h>
#include <lusb0_usb.h>
#include <usb.h>


// return result string
static char result[80]="";

// driver name
#define DRIVER_NAME "LCD2USB Display DLL V0.13"

// usage
#define USAGE "no parameters"

/* vendor and product id */
#define LCD2USB_VID  0x0403
#define LCD2USB_PID  0xc630

/* target is a bit map for CMD/DATA */
#define LCD_CTRL_0         (1<<3)
#define LCD_CTRL_1         (1<<4)
#define LCD_BOTH           (LCD_CTRL_0 | LCD_CTRL_1)

#define LCD_ECHO           (0<<5)
#define LCD_CMD            (1<<5)
#define LCD_DATA           (2<<5)
#define LCD_SET            (3<<5)
#define LCD_GET            (4<<5)

/* target is value to set */
#define LCD_SET_CONTRAST   (LCD_SET | (0<<3))
#define LCD_SET_BRIGHTNESS (LCD_SET | (1<<3))
#define LCD_SET_RESERVED0  (LCD_SET | (2<<3))
#define LCD_SET_RESERVED1  (LCD_SET | (3<<3))

/* target is value to get */
#define LCD_GET_FWVER      (LCD_GET | (0<<3))
#define LCD_GET_KEYS       (LCD_GET | (1<<3))
#define LCD_GET_CTRL       (LCD_GET | (2<<3))
#define LCD_GET_RESERVED1  (LCD_GET | (3<<3))

static usb_dev_handle *handle = NULL;
static int controllers = 0, ctrlr = 0;
static int rows = 0, columns = 0;
static int xpos = 0, ypos = 0;
static int bright = 0;


// internal functions

static int lcd_send(int request, int value, int index) {
	if(usb_control_msg(handle, USB_TYPE_VENDOR, request, 
		value, index, NULL, 0, 1000) < 0) {
			return -1;
	}
	return 0;
}

/* to increase performance, a little buffer is being used to */
/* collect command bytes of the same type before transmitting them */
#define BUFFER_MAX_CMD 4        /* current protocol supports up to 4 bytes */
static int buffer_current_type = -1;   /* nothing in buffer yet */
static int buffer_current_fill = 0;    /* -"- */
static unsigned char buffer[BUFFER_MAX_CMD];

/* command format:
* 7 6 5 4 3 2 1 0
* C C C T T R L L
*
* TT = target bit map
* R = reserved for future use, set to 0
* LL = number of bytes in transfer - 1
*/

/* flush command queue due to buffer overflow / content */
/* change or due to explicit request */
static void lcd_flush(void) {
	int request, value, index;

	/* anything to flush? ignore request if not */
	if (buffer_current_type == -1)
		return;

	/* build request byte */
	request = buffer_current_type | (buffer_current_fill - 1);

	/* fill value and index with buffer contents. endianess should IMHO not */
	/* be a problem, since usb_control_msg() will handle this. */
	value = buffer[0] | (buffer[1] << 8);
	index = buffer[2] | (buffer[3] << 8);

	/* send current buffer contents */
	lcd_send(request, value, index);

	/* buffer is now free again */
	buffer_current_type = -1;
	buffer_current_fill = 0;
}

/* enqueue a command into the buffer */
static void lcd_enqueue(int command_type, int value) {
	if ((buffer_current_type >= 0) && (buffer_current_type != command_type))
		lcd_flush();

	/* add new item to buffer */
	buffer_current_type = command_type;
	buffer[buffer_current_fill++] = value;

	/* flush buffer if it's full */
	if (buffer_current_fill == BUFFER_MAX_CMD)
		lcd_flush();
}

/* see HD44780 datasheet for a command description */
static void lcd_command(const unsigned char ctrl, const unsigned char cmd) {
	lcd_enqueue(LCD_CMD | ctrl, cmd);
}

/* clear display */
static void lcd_clear(void) {
	xpos = ypos = 0;
	ctrlr = LCD_CTRL_0;
	lcd_command(LCD_BOTH, 0x01);    /* clear display */
	lcd_command(LCD_BOTH, 0x03);    /* return home */
}

/* home display */
static void lcd_home(void) {
	xpos = ypos = 0;
	ctrlr = LCD_CTRL_0;
	lcd_command(LCD_BOTH, 0x03);    /* return home */
}

/* write a data string */
static void lcd_write(const unsigned char ctrl, const unsigned char *data) {
	int c;

	while((xpos++ < columns) && (c = (int)*data++)) {

		// translation pattern for custom characters
		switch (c) {
			case 176: c = 0; break;
			case 158: c = 1; break;
			case 131: c = 2; break;
			case 132: c = 3; break;
			case 133: c = 4; break;
			case 134: c = 5; break;
			case 135: c = 6; break;
			case 136: c = 7; break;
			default: break;
		}

		lcd_enqueue(LCD_DATA | ctrl, c);
	}

	lcd_flush();
}

/* set a value in the LCD interface */
static void lcd_set(unsigned char cmd, int value) {
	if(usb_control_msg(handle, 
		USB_TYPE_VENDOR, cmd, value, 0, 
		NULL, 0, 1000) < 0) {
	}
}

/* get a value from the lcd2usb interface */
static int lcd_get(unsigned char cmd) {
	unsigned char       buffer[2];
	int                 nBytes;

	/* send control request and accept return value */
	nBytes = usb_control_msg(handle, 
		USB_TYPE_VENDOR | USB_RECIP_DEVICE | USB_ENDPOINT_IN, 
		cmd, 0, 0, (char *)buffer, sizeof(buffer), 1000);

	if(nBytes < 0) {
		return -1;
	}

	return buffer[0] + 256*buffer[1];
}

static void lcd_setpos(unsigned char x, unsigned char y)
{
	int pos;

	xpos = x;
	ypos = y;
	
	/* displays with more two rows and 20 columns have a logical width */
	/* of 40 chars and use more than one controller */
	if ((rows > 2) && (columns > 20) && (y > 1)) {
		/* use second controller */
		y -= 2;
		ctrlr = LCD_CTRL_1;
	} else
		/* use first controller */
		ctrlr = LCD_CTRL_0;


	/* 16x4 Displays use a slightly different layout */
	if ((rows == 4) && (columns == 16)) {
		pos = (y % 2) * 64 + (y / 2) * 16 + x;
	} else {
		pos = (y % 2) * 64 + (y / 2) * 20 + x;
	}

	lcd_command(ctrlr, 0x80 | pos);
}

static void lcd_custchar(const int ascii, unsigned char *matrix)
{
	int i;
	unsigned char mybuf[8];

	lcd_command(LCD_BOTH, 0x40 | 8 * ascii);

	for (i = 0; i < 8; i++) {
		mybuf[i] = *matrix;
		lcd_enqueue(LCD_DATA | LCD_BOTH, *matrix++ & 0x1f);
	}

	lcd_flush();
}

//------

// Exported DLL fucntions

// Name
char * __stdcall  DISPLAYDLL_DriverName(void)
{
	return DRIVER_NAME;
}

// Usage
char * __stdcall  DISPLAYDLL_Usage(void)
{
	return USAGE;
}

// Init
char * __stdcall  DISPLAYDLL_Init(const BYTE x_size, const BYTE y_size, const unsigned char *startparam, bool *ok)
{
	struct usb_bus      *bus;
	struct usb_device   *dev;

	usb_init();

	usb_find_busses();
	usb_find_devices();

	for(bus = usb_get_busses(); bus; bus = bus->next) {
		for(dev = bus->devices; dev; dev = dev->next) {
			if((dev->descriptor.idVendor == LCD2USB_VID) && 
				(dev->descriptor.idProduct == LCD2USB_PID)) {

					/* open device */
					handle = usb_open(dev);

					break;
			}
		}
	}

	if (handle == NULL) {
		*ok = FALSE;
		strcpy_s(result, "no USB2LCD-Device found");
		controllers = 0;  // don't access any controllers
	} else {
		controllers = lcd_get(LCD_GET_CTRL);

		if (controllers != -1) {
			if (!controllers) {
				*ok = FALSE;
				strcpy_s(result, "no controllers found");
				controllers = 0;  // don't access any controllers
			} else {
				*ok = FALSE;
				strcpy_s(result, "unable to read installed controllers");
				controllers = 0;  // don't access any controllers
			}

			// convert into controller map matching our protocol
			controllers = ((controllers & 1) ? LCD_CTRL_0 : 0) | ((controllers & 2) ? LCD_CTRL_1 : 0);
			rows = y_size;
			columns = x_size;
			lcd_clear();
			*ok = TRUE;
			strcpy_s(result, "OK");
		}
	}
	return result;
}

// Fini
void __stdcall  DISPLAYDLL_Done(void)
{
	if (handle != NULL) {
		usb_close(handle);
	}
}

void __stdcall  DISPLAYDLL_SetBacklight(const bool on)
{
	if (on)
		lcd_set(LCD_SET_BRIGHTNESS, bright);
	else
		lcd_set(LCD_SET_BRIGHTNESS, 0);
}


// Contrast
void __stdcall  DISPLAYDLL_SetContrast(const BYTE val)
{
	lcd_set(LCD_SET_CONTRAST, val);
}

// Brightness
void __stdcall  DISPLAYDLL_SetBrightness(const BYTE val)
{
	bright = val;
	lcd_set(LCD_SET_BRIGHTNESS, val);
}

// write a string
void __stdcall  DISPLAYDLL_Write(const unsigned char *str)
{
	lcd_write(ctrlr, str);
}

// set cursor position
void __stdcall  DISPLAYDLL_SetPosition(const BYTE x, const BYTE y)
{
	lcd_setpos(x - 1, y - 1);
}

// define custom character
void __stdcall  DISPLAYDLL_CustomChar(const BYTE ascii, unsigned char *bitmap)
{
	lcd_custchar(ascii - 1, bitmap);
}
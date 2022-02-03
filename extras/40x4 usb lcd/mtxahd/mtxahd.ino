// heavily modified to support lcd smartie with a 40x4 8bit display
#include "LiquidCrystal.h"

// 40x4 set it up like a 40x2
const int numRows = 2;
const int numCols = 40;

// 40x4 has a controller for lines 1 and 2 and another for lines 3 and 4
const int rs = 18, rw = 15, en1 = 14, en2 = 16, d0 = 9, d1 = 8, d2 = 7, d3 = 6, d4 = 5, d5 = 4, d6 = 3, d7 = 2;
LiquidCrystal lcd1(rs, rw, en1, d0, d1, d2, d3, d4, d5, d6, d7);
LiquidCrystal lcd2(rs, rw, en2, d0, d1, d2, d3, d4, d5, d6, d7);


int controller;
int incoming;
byte rxbyte;
byte temp;
byte custompos;
byte customchar[8];


void setup() {
  analogWrite(10,100); 
  Serial.begin(115200);
  // set up the LCD's number of rows and columns: 
  lcd1.begin(numCols, numRows);
  lcd2.begin(numCols, numRows);
  
  // some startup text
  lcd1.setCursor(0,0);
  lcd1.print("            HD44780  ARDUINO            ");
  lcd1.setCursor(0,1);
  lcd1.print("              LCD  SMARTIE              ");
  lcd2.setCursor(0,0);
  lcd2.print("                INIT  OK                ");
}

byte serial_getch(){   
  while (Serial.available()==0){}
  incoming = Serial.read();
  return (byte) (incoming &0xff);
}

void loop(){
  rxbyte = serial_getch();
  if (rxbyte == 0xFE)
  {
    temp=serial_getch();
    switch (temp)
    {
    case 71:  //set cursor position
      temp = (serial_getch() - 1);  //get column byte
      switch (serial_getch())  //get row byte
      {
        case 1:
          controller=1;
          lcd1.command(0b10000000 + temp);
        break;
        case 2:
          controller=1;
          temp += 0x40;
          lcd1.command(0b10000000 + temp);
        break;
        case 3:
          controller=2;
          lcd2.command(0b10000000 + temp);
        break;
        case 4:
          controller=2;
          temp += 0x40;
          lcd2.command(0b10000000 + temp);      
        break;
      }
    break;
	
    case 78:  //define custom char
      custompos = serial_getch();
      for (temp = 0; temp != 8; temp++)
      {
        customchar[temp]=serial_getch(); //get each pattern byte
      }
      lcd1.createChar(custompos, customchar);
      lcd2.createChar(custompos, customchar);
    break;

    case 70: //backlight off // dont support
    break;

    default: // gpo, brightness, contrast and everything else dont support and discard parameter
     temp = serial_getch();
    break;
    }
    return;
  }

  switch (controller)
  {
    case 1:
    lcd1.write(rxbyte);
    break;
    case 2:
    lcd2.write(rxbyte);
    break;
  }
  return;
}

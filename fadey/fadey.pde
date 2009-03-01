/* -*- mode: c -*-

   fades some LEDs.
   Hardware needs to be: PWM pin -> resistor -> LED -> GND

 */

int indicator = 13;
int dialPin = 0;
int DISPLAY_SIZE = 5;
int pinDigits[] = {3, 5, 6, 9, 10};

void setup(){
  pinMode(indicator, OUTPUT);
}

void ledArrayOut(int value){
  /*
    need to figure out what bracket the value falls in,
    and light up LEDs based on how close they are to that bracket.
   */

  for(int i = 0; i < DISPLAY_SIZE; i++){
    int pos = i * 255;
    int val = 0;
    if(abs(value - pos) <= 255){
      //this should be on, dimmed based upon value's distance from the LED
      //brighter if we're closer
      val = 255 - (value >= pos ? value - pos : pos - value);
    }
    analogWrite(pinDigits[i], val);
  }
}

void loop (){
  ledArrayOut(analogRead(dialPin));  
}

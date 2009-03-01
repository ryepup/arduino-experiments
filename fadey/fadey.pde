/* -*- mode: c -*-

   fades some LEDs.
   Hardware needs to be: PWM pin -> resistor -> LED -> GND

 */

int indicator = 13;
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
      val = value > pos ? pos - value : value - pos;
    }


    analogWrite(pinDigits[i], val);
  }
  delay(10);
}

void loop (){
  digitalWrite(indicator, HIGH);
  int maxValue = DISPLAY_SIZE * 255;
  for(int i = 0; i < maxValue; i++)
    ledArrayOut(i);
  digitalWrite(indicator, LOW);
  for(int i = maxValue; i; i--)
    ledArrayOut(i);
}

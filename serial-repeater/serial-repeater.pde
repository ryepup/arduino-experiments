// -*- mode: c -*-
int ledPin = 13;
int blinkMs = 2000;
int inByte = 0;
int on = 0;

void setup(){
  pinMode(ledPin, OUTPUT);
  Serial.begin(19200);
}

void blink (int times, int ms){
  for(int i = 0; i < times; i++){
      digitalWrite(ledPin, HIGH);
      delay(ms);
      digitalWrite(ledPin, LOW);
      delay(ms);
  }
}


void loop (){
  if(Serial.available() > 0){
    blink(5,250);
    inByte = Serial.read();
    Serial.flush();
  }
  blink(2,250);
  delay(1000);
}


extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {} 

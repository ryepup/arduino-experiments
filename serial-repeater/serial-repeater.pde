// -*- mode: c -*-
int ledPin = 13;
int blinkMs = 2000;

void setup(){
  pinMode(ledPin, OUTPUT);
  Serial.begin(9600);
}

void loop (){
  digitalWrite(ledPin, HIGH);
  Serial.println("Hello");
  delay(blinkMs);
  digitalWrite(ledPin, LOW);
  delay(blinkMs);
}

extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {} 

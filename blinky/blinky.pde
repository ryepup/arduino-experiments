// -*- mode: c -*-
int ledPin = 13;
int blinkMs = 2000;

void setup(){
  pinMode(ledPin, OUTPUT);
}

void loop (){
  digitalWrite(ledPin, HIGH);
  delay(blinkMs);
  digitalWrite(ledPin, LOW);
  delay(blinkMs);
}

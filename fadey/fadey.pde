// -*- mode: c -*-
int ledPin = 13;
int fadingPin = 9;
int pins[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};  

void setup(){
  pinMode(ledPin, OUTPUT);
}


void fade(int to, int delayMs){
  int d = to > pins[fadingPin] ? 1 : -1;
  for(;pins[fadingPin] != to; pins[fadingPin] += d){
    analogWrite(fadingPin, pins[fadingPin]);
    delay(delayMs);
  }
}

void loop (){
  digitalWrite(ledPin, HIGH);
  fade(255, 30);
  digitalWrite(ledPin, LOW);
  fade(0, 30);
}


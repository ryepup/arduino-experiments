// -*- mode: c -*-
int ledPin = 13;
int inByte = 0;
int on = 0;

void blink (int times, int ms){
  for(int i = 0; i < times; i++){
      digitalWrite(ledPin, HIGH);
      delay(ms);
      digitalWrite(ledPin, LOW);
      delay(ms);
  }
}

void setup(){
  pinMode(ledPin, OUTPUT);
  Serial.begin(9600);
  blink(5,100);
}

int nextByte(){
  while(Serial.available() == 0) {}
  return Serial.read();
}

void doAnalogRead(){
  int pin = nextByte();
  int val = analogRead(pin);
  Serial.write(val);
}

void doAnalogWrite(){
  int pin = nextByte();
  int val = nextByte();
  analogWrite(pin, val);
}


void doDigitalWrite(){
  int pin = nextByte();
  int val = nextByte();
  digitalWrite(pin, val > 128 ? HIGH : LOW);
}

void loop (){
  switch(nextByte()){
  case 200:
    doAnalogRead();
    break;
  case 205:
    doAnalogWrite();
    break;
  case 210:
    doDigitalWrite();
    break;
  }
  Serial.write(1);
  Serial.flush();
}

extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {} 

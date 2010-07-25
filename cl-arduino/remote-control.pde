// -*- mode: c -*-
int ledPin = 13;
int inByte = 0;
int on = 1;
int irPin = 7;

#define IR_BIT_LENGTH 16    // number of bits sent by IR remote
#define BIT_1 1000          // Binary 1 threshold (Microseconds)
#define BIT_0 200           // Binary 0 threshold (Microseconds)
#define BIT_START 2000      // Start bit threshold (Microseconds)


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
  pinMode(irPin, INPUT);
  Serial.begin(9600);
  blink(5,100);
}

int get_ir_key(int irPin) 
{

  do {} //Wait for a start bit
  while(pulseIn(irPin, LOW) < BIT_START);

  int result = 0;
  int seed = 1;
  for (int i = 0; i < IR_BIT_LENGTH; i++)
  {
    if (pulseIn(irPin, HIGH) > BIT_1){
      result += seed;
    }
    seed *= 2;
  }
  return result;
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

void doIrRead(){
  int pin = nextByte();
  int code = get_ir_key(pin);
  Serial.println(code);
}

void loop (){
  if(on){
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
  case 215:
    doIrRead();
    break;
  case 220:
    on = 0;
    blink(5,100);
    break;
  }
  Serial.write(1);
  Serial.flush();
  }else{
    delay(1000*100);
  }
}

extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {} 

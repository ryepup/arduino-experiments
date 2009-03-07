/* -*- mode: c -*-

Simple pass-through to send pin states up via serial when they change.

 */

int analogPins[] = {0,1,2,3,4,5};
int analogPinValues[] = {0,0,0,0,0,0};
float analogPinAverageDiff[] = {0,0,0,0,0,0};
long analogPinAverageN[] = {0,0,0,0,0,0};

void setup(){
  Serial.begin(9600);
}

void pinDiff(int idx){
  int pin, oldValue, newValue, diff;
  long n;

  pin = analogPins[idx];
  oldValue = analogPinValues[idx];
  newValue = analogRead(pin);
  diff = newValue-oldValue;
  if(abs(diff) < 2) return;

  analogPinValues[idx] = newValue;
  n = analogPinAverageN[idx]++;
  analogPinAverageDiff[idx] += (newValue - oldValue) / (float) n;


  Serial.print("Pin ");
  Serial.print(pin);
  Serial.print(": ");
  Serial.print(oldValue);
  Serial.print(" -> ");
  Serial.print(newValue);
  for(int i = 0; i < abs(diff); i++){
    Serial.print( (diff > 0) ? "+" : "-");
  }

  Serial.println("");
}

void loop (){
  for(int i = 0; i<6;i++){
    pinDiff(i);
  }
}

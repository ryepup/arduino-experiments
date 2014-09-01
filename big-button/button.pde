/* -*- mode: c -*-
Based on  http://www.arduino.cc/en/Tutorial/Debounce
 */

// Constants
const int buttonPin = 2;    // the number of the pushbutton pin
const int ledPin = 13;      // the number of the LED pin
const long DEBOUNCE_DELAY = 50; // minimum press to be considered a press

// Variables
int down = HIGH; // is the button down (LOW) or up (HIGH)
long lastDebounceTime = 0;  // the last time the output pin was toggled

void setup() {
  pinMode(buttonPin, INPUT);
  pinMode(ledPin, OUTPUT);

  // set initial LED state
  digitalWrite(ledPin, LOW);
  Serial.begin(9600);
}

void loop() {
  int is_pushed = digitalRead(buttonPin) == HIGH;
  int is_down = down == LOW;
  long now = millis();

  if (is_pushed && !is_down){
    down = LOW;
    lastDebounceTime = now;
    digitalWrite(ledPin, HIGH);
  }
  else if (!is_pushed && is_down) {
    down = HIGH;
    digitalWrite(ledPin, LOW);
    // were we down long enough to be considered a push?
    long duration = (now - lastDebounceTime);
    if(duration > DEBOUNCE_DELAY){
      Serial.println(duration);
    } 
  }
}

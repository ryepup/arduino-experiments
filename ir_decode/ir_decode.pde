/* -*- mode: c -*- */
// 0.1 by pmalmsten http://www.arduino.cc/cgi-bin/yabb2/YaBB.pl?num=1176098434
// 0.2 by farkinga
// 0.3 by farkinga - adds cool behaviors

#define IR_BIT_LENGTH 32    // number of bits sent by IR remote
#define BIT_1 1000          // Binary 1 threshold (Microseconds)
#define BIT_0 400           // Binary 0 threshold (Microseconds)
#define BIT_START 2000      // Start bit threshold (Microseconds)

#define IR_PIN 7            // Sensor pin 1 wired through a 220 ohm resistor
#define LED_PIN 9           // first LED output
#define POWER_PIN 11        // second LED output, corresponds to power button

#define DEBUG 0             // Serial connection must be started to debug

int runtime_debug = 1;      // flag to output raw IR pulse data
int output_key = 1;         // flag to print decoded key integers
int power_button = 0;       // flag to indicate if power LED is on
int power_level = 128;      // value (0-255) for power LED intensity

void setup() {
  pinMode(LED_PIN, OUTPUT);	//This shows when we're ready to recieve
  pinMode(POWER_PIN, OUTPUT);	//This is the "power on" indicator
  pinMode(IR_PIN, INPUT);
  digitalWrite(LED_PIN, LOW);
  Serial.begin(9600);
}


/* 
  make LED blink rapidly
*/

void blink_led()
{
  for (int i = 0; i < 5; i++)
  {
    analogWrite(LED_PIN, 0);
    delay(50);
    analogWrite(LED_PIN, 1);
    delay(50);
  }
}

/*
  use pulseIn to receive IR pulses from the remote.
  Record the length of these pulses (in ms) in an array
*/

void read_pulse(int pulse[], int num_bits)
{
  for (int i = 0; i < num_bits; i++)
  {
    pulse[i] = pulseIn(IR_PIN, LOW);
  }
}

/*
  IR pulses encode binary "0" as a short pulse, and binary "1"
  as a long pulse.  Given an array containing pulse lengths,
  convert this to an array containing binary values
*/

void pulse_to_bits(int pulse[], int bits[], int num_bits)
{
  if (DEBUG || runtime_debug) { Serial.println("-----"); }
  
  for(int i = 0; i < num_bits ; i++) 
  {
    if (DEBUG || runtime_debug) { Serial.print(i); Serial.print(":"); Serial.println(pulse[i]); }
    
    if(pulse[i] > BIT_1) //is it a 1?
    {
      bits[i] = 1;
    }  
    else if(pulse[i] > BIT_0) //is it a 0?
    {
      bits[i] = 0;
    } 
    else //data is invalid...
    {
      Serial.println("Error");
    }
  }
}

/*
  convert an array of binary values to a single base-10 integer
*/

int bits_to_int(int bits[], int num_bits)
{
  int result = 0;
  int seed = 1;
  
  //Convert bits to integer
  for(int i = 0 ; i < num_bits ; i++) 
  {		  
    if(bits[i] == 1) 
    {
	result += seed;
    }
    
    seed *= 2;
  }
  
  return result;
}

/*
  set the brightness of the "power LED" depending on the power_level
  variable.
*/

void set_power()
{
  power_level = constrain(power_level, 0, 200);
  
  analogWrite(POWER_PIN, power_level * power_button);
  
  // if the power level is above the max or below the min...
  if ((power_level == 200) || (power_level == 0))
  {
    blink_led();
  }
}

/*
  neat little routine to fade both LEDs in a sequence.  Currently,
  this is called by do_response() when the "play" button is pressed.
*/

void blip_power()
{
  int max_val = 100;
  
  for (int i = 0; i < max_val; i++)
  {
    analogWrite(POWER_PIN, max_val-i);
    analogWrite(LED_PIN, i);
    delay(15);
  }

  for (int i = max_val; i >= 0; i--)
  {
    analogWrite(POWER_PIN, max_val-i);
    analogWrite(LED_PIN, i);
    delay(15);
  }

  set_power();  
}


/*
  wait for a keypress from the IR remote, and return the
  integer mapping of that key (e.g. power button on remote returns 
  the integer 1429)
*/

int get_ir_key() 
{
  int pulse[IR_BIT_LENGTH];
  int bits[IR_BIT_LENGTH];  

  do {} //Wait for a start bit
  while(pulseIn(IR_PIN, LOW) < BIT_START);

  read_pulse(pulse, IR_BIT_LENGTH);
  pulse_to_bits(pulse, bits, IR_BIT_LENGTH);
  return bits_to_int(bits, IR_BIT_LENGTH);
}


/* 
  respond to specific remote-control keys with different behaviors
*/

void do_response(int key)
{  
  switch (key)
  {
    case 1437:  // record button
      Serial.println("toggle debug pulse");
      runtime_debug = 1 - runtime_debug;
      break;
    case 1498:  // display button
      Serial.println("Toggle key output");
      output_key = 1 - output_key;
      break;
    case 1429:  // power button
      Serial.println("Power");
      power_button = 1 - power_button;
      set_power();
      break;
    case 1424:  // channel up button
      Serial.println("Channel Up");
      break;      
    case 1425:  // channel down button
      Serial.println("Channel Down");
      break;
    case 3342:  // up rocker/pause
      power_level+=1;
      set_power();
      break;
    case 3343:  // down rocker/stop
      power_level-=1;
      set_power();
      break;      
    case 3344:  // left rocker/rewind
      if (power_level < 50)
      {
        power_level-=3;
      }
      else
      {
        power_level-=10;
      }
      set_power();
      break;
    case 3345:  // right rocker/fast forward
      if (power_level < 50)
      {
        power_level+=3;
      }
      else
      {
        power_level+=10;
      }
      set_power();
      break;
    case 3352:  // play button
      blip_power();
      break;      
    default:
      if (output_key)
      {
        Serial.print("Key ");
        Serial.print(key);
        Serial.println(" not programmed");
      }
      break;
  }
}

void loop() {
  digitalWrite(LED_PIN, HIGH);
  int key = get_ir_key();
  
  digitalWrite(LED_PIN, LOW);  // turn LED off while processing response
  do_response(key);
  delay(100);                  // short delay to cancel duplicate keypresses
}

extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {} 

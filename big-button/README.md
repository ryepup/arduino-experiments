# big button

This small program is intended to hook up to a variety of other
programs and provide a simple physical interface

Components:

* arduino
* momentary button switch
* python driver

# Usage:

    ./big_button.py /dev/ttyUSB0

The program will print to standard out when the button is pushed. You
can pipe the output to any other program, and act accordingly.

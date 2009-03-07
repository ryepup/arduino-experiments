import serial

def listen():
    global ser
    ser = serial.Serial('/dev/ttyUSB0', 9600) 

def watch():
    global ser
    while 1:
        print(ser.readline())


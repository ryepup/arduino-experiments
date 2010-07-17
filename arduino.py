import serial

def listen():
    global ser
    ser = serial.Serial('/dev/ttyUSB0', 9600) 

def watch():
    global ser
    while 1:
        print ser.readline(),


if __name__ == '__main__':
    global ser
    listen()
    try:
        watch()
    finally:
        ser.close()

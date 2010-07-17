import serial

def listen():
    global ser
    ser = serial.Serial('/dev/ttyUSB0', 9600) 

def watch():
    global ser
    readings = {}
    while 1:
        reading = ser.readline().split(":")
        if len(reading) == 1:                    
            print ser.readline(),
        else:
            bit = reading[0]
            value = int(reading[1])
            is_max = False
            is_min = False
            if readings.has_key(bit):
                readings[bit]["sum"] += value
                readings[bit]["count"] += 1
                if value < readings[bit]["min"]:
                    readings[bit]["min"] = value
                    is_min = True
                if value > readings[bit]["max"]:
                    readings[bit]["max"] = value
                    is_max = True
            else:
                readings[bit] = {"sum":value, "count":1, "min":value, "max":value}
            avg = readings[bit]["sum"] / readings[bit]["count"]
            print "[%s] = %s; avg: %s, %s - %s, %s %s" % (bit, value, avg, readings[bit]["min"], readings[bit]["max"], is_max, is_min)


if __name__ == '__main__':
    global ser
    listen()
    try:
        watch()
    finally:
        ser.close()

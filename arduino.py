import serial
import sys
import argparse
import logging

log = logging.getLogger('arduino')

parser = argparse.ArgumentParser(
    description='Listen for serial communication from an arduino, and repeat it')

parser.add_argument('--port', default='/dev/ttyUSB0',
                   help='serial port to listen on')
parser.add_argument('--baud', default=9600)

if __name__ == '__main__':
    args = parser.parse_args()
    logging.basicConfig(level=logging.DEBUG)
    ser = serial.Serial(args.port, args.baud)
    try:
        while True:
            line = ser.readline().strip()
            log.info(line)
            print line
            sys.stdout.flush()
    except KeyboardInterrupt:
        pass
    finally:
        ser.close()

import sys
import logging
import argparse
import subprocess

log = logging.getLogger(__name__)

mpd_host = None

def status():
    res = subprocess.check_output(['mpc', '--host', mpd_host, 'status'])
    return { 
        'paused': '[paused]' in res,
        'playing': '[playing]' in res
    }

def play():
    subprocess.check_call(['mpc', '--host', mpd_host, 'play'])

def pause():
    subprocess.check_call(['mpc', '--host', mpd_host, 'pause'])

def wait_for_button(f):
    log.info('wait_for_button')
    press_ms = int(f.readline().strip())
    log.info('button pressed for %s ms', press_ms)
    s = status()
    if s['paused']:
        play()
    elif s['playing']:
        pause()


parser = argparse.ArgumentParser(description='mpd button controller')
parser.add_argument('--mpd', help='MPD host', default='localhost') 

if __name__ == '__main__':
    args = parser.parse_args()
    mpd_host = args.mpd
    logging.basicConfig(level=logging.DEBUG)
    log.info('listening for presses')
    try:
        while True:
            wait_for_button(sys.stdin)
    except KeyboardInterrupt:
        pass

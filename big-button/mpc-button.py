import sys
import logging
import argparse
import subprocess
import os.path

log = logging.getLogger(__name__)

mpd_host = None

def status():
    res = subprocess.check_output(['mpc', '--host', mpd_host, 'status'])
    return { 
        'paused': '[paused]' in res,
        'playing': '[playing]' in res
    }

def play(i=1):
    subprocess.check_call(['mpc', '--host', mpd_host, 'play', str(i)])

def pause():
    subprocess.check_call(['mpc', '--host', mpd_host, 'pause'])

def toggle():
    subprocess.check_call(['mpc', '--host', mpd_host, 'toggle'])

def playlist():
    return subprocess.check_output(['mpc', '--host', mpd_host, '-f',
                                    '%title%', 'playlist'])

def album_search(q):
    res = subprocess.check_output(['mpc', '--host', mpd_host, 'search',
                                    'album', q])
    return res.split('\n')[0]

def add_and_play(folder, i):
    ps = subprocess.Popen(['mpc', '--host', mpd_host, 'ls', folder],
                          stdout=subprocess.PIPE)
    subprocess.check_call(['mpc', '--host', mpd_host, 'add'], stdin=ps.stdout)
    ps.wait()
    play(i)

def enqueue_related():
    words = []
    current_playlist = playlist().split('\n')
    for title in current_playlist:
        for w in title.split():
            words.append(w.lower())

    words.reverse()
    for q in words:
        album = album_search(q)
        if album:
            folder= os.path.dirname(album)
            add_and_play(folder, len(current_playlist))
            return

def wait_for_button(f):
    log.info('wait_for_button')
    press_ms = int(f.readline().strip())
    log.info('button pressed for %s ms', press_ms)
    s = status()
    if s['paused'] or s['playing']:
        toggle()
    else:
        enqueue_related()


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

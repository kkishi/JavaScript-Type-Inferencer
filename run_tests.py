import os, os.path
import re
import commands

def StatusStr(status_code):
    return '' if status_code == 0 else 'ERR'

for root, dirs, files in os.walk('.'):
    for file in files:
        if re.match('.*\.js', file):
            path = os.path.join(root, file)
            res = commands.getstatusoutput('./infer.py -pal < ' + path)
            print(file + ' ' + StatusStr(res[0]))

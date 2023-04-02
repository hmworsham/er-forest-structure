
import pandas as pd

thing = open()

with open('/Users/hmworsham/Desktop/waveform_binary_drive_ids.txt') as f:
    lines = f.readlines()

ids = [x for xs in lines for x in xs.split(', ')]
ids = [x.split('/')[5] for x in ids]
ids

for i in ids:
    xx = 'https://drive.google.com/uc?export=download&id='+i
    print(xx)


import wget
import ssl
for i in ids:
    fileid = 'https://drive.google.com/uc?export=download&id='+i
    filename = '/Volumes/Brain/GIS/RMBL/NEON_AOP_2018/Waveform_LiDAR/'+i+'.7z'
    ssl._create_default_https_context = ssl._create_unverified_context
    wget.download(fileid, filename)



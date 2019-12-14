import json
import pandas as pd
import numpy as np

with open('wp_parenting.txt', 'r') as f:
    data = json.load(f)

dates = []
for thread in data['wp.pl']['threads']:
		for post in thread['posts']:
			dates.append(post['date'])
			
pd_dates = dates.astype('datetime64')
pd_dates.groupby(pd_dates.dt.date).count()

#!/usr/bin/python

import json
from multiprocessing.pool import ThreadPool
import re
import sys
import urllib.request

query = sys.argv[1]
limit = int(sys.argv[2]) if sys.argv[2] else 25
offset = int(sys.argv[3]) if sys.argv[3] else 0

packages = urllib.request.urlopen("https://packages.octave.org/list_packages.php").read().decode("utf-8").split()

# Apply query, limit, and offset to packages
if query:
    packages = [x for x in packages if query.lower() in x.lower()]
packages = packages[offset:(offset + limit)]

def get_package_details(name):
    page = urllib.request.urlopen("https://packages.octave.org/%s/index.html" % name).read().decode("utf-8")

    try:
        version = re.search('<td class="package_table">Package Version:</td>\s*<td>([\d.]*)</td>', page).group(1)
    except:
        version = ""

    try:
        description = re.search('<h3>Description</h3>\s*<p>\s*(.*)\s*</p>', page).group(1)
    except:
        description = ""

    return (version, description)

def go(x):
    try:
        (version, description) = get_package_details(x)
    except:
        (version, description) = ("unknown", "")

    return {"name": x, "version": version, "description": description}

with ThreadPool(50) as p:
    print(json.dumps(p.map(go, packages)))

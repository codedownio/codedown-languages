from collections import namedtuple
import json, os, sys
from pip._internal.commands import create_command

query = sys.argv[1]
limit = int(sys.argv[2]) if sys.argv[2] else 25
offset = int(sys.argv[3]) if sys.argv[3] else 0

pypiIndex = "https://pypi.org/pypi"

cacheDir = os.path.join(os.environ["HOME"], ".cache/python-package-manager")
if not os.path.exists(cacheDir):
    os.makedirs(cacheDir)

OptionsTuple = namedtuple(
    "Options",
    [
        "index",
        "cache_dir",
        "retries",
        "trusted_hosts",
        "cert",
        "client_cert",
        "timeout",
        "proxy",
        "no_input",
    ],
)
options = OptionsTuple(pypiIndex, cacheDir, 3, [], None, None, None, None, True)

command = create_command("search")
with command.main_context():
    results = command.search(query, options)

results = results[offset : (offset + limit)]

for result in results:
    result["description"] = result["summary"]
    del result["summary"]

print(json.dumps(results))

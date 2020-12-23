import os
import subprocess
import signal
import sys

import requirements

NAME = sys.argv[1]

originalTopRequirements = ""
newTopRequirements = []
if os.path.exists("top-requirements.txt"):
    with open("top-requirements.txt", "r") as f:
        originalTopRequirements = f.read()
        f.seek(0)

        for req in requirements.parse(f):
            if req.name == NAME:
                continue
            newTopRequirements.append(req.name)


def revert_top_requirements():
    with open("top-requirements.txt", "w") as f:
        f.write(originalTopRequirements)


# Catch interrupts and fail the install
def signal_handler(sig, frame):
    print("\n\nRemoval cancelled by SIGINT")
    revert_top_requirements()
    sys.exit(1)


signal.signal(signal.SIGINT, signal_handler)

with open("top-requirements.txt", "w") as f:
    f.write("\n".join(newTopRequirements))

print("pip uninstall -y %s" % NAME)
result = subprocess.Popen(["pip", "uninstall", "-y", NAME])
result.wait()
if result.returncode != 0:
    revert_top_requirements()
    sys.exit(result.returncode)

with open("requirements.txt", "w") as f:
    print("pip freeze > requirements.txt")
    result = subprocess.Popen(["pip", "freeze"], stdout=f)
    result.wait()
    sys.exit(result.returncode)

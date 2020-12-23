import os
import subprocess
import signal
import sys

NAME = sys.argv[1]
VERSION = sys.argv[2]

originalTopRequirements = ""
if os.path.exists("top-requirements.txt"):
    with open("top-requirements.txt", "r") as f:
        originalTopRequirements = f.read()


def revert_top_requirements():
    with open("top-requirements.txt", "w") as f:
        f.write(originalTopRequirements)


# Catch interrupts and fail the install
def signal_handler(sig, frame):
    print("\n\nInstallation cancelled by SIGINT")
    revert_top_requirements()
    sys.exit(1)


signal.signal(signal.SIGINT, signal_handler)

with open("top-requirements.txt", "a") as f:
    f.write("\n" + NAME)

try:
    rows, columns = os.popen("stty size", "r").read().split()
    columns = int(columns)
except:
    columns = float("inf")

progress_args = [] if columns >= 80 else ["--progress-bar", "off"]

command = [
    "pip",
    "install",
    "--user",
    "-r",
    "top-requirements.txt",
] + progress_args
print(" ".join(["pip"] + command[1:]))
result = subprocess.Popen(command)
result.communicate()
if result.returncode != 0:
    print("Exception while trying to install package. Reverting top-requirements.txt")
    revert_top_requirements()
    sys.exit(result.returncode)

with open("requirements.txt", "w") as f:
    print("pip freeze > requirements.txt")
    result = subprocess.Popen(["pip", "freeze"], stdout=f)
    result.communicate()
    sys.exit(result.returncode)

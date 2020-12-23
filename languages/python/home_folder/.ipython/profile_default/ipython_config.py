
# This file contains some custom settings for the Python language kernel.
# You can add your own settings here, too! Make sure to restart the kernel
# in order to pick up changes.

# Note that this file is a symlink to ~/.ipython/profile_default/ipython_config.py

c = get_config()

# Set "%matplotlib inline" so that plots appear inline
c.IPKernelApp.matplotlib = "inline"

# Load the "autoreload" extension and set "%autoreload 2"
# This allows us to edit code in .py files and then use it immediately in the
# notebook without re-evaluating the .py file.
# https://ipython.readthedocs.io/en/stable/config/extensions/autoreload.html
c.IPKernelApp.extensions = ["autoreload"]
c.IPKernelApp.exec_lines = ["%autoreload 2"]

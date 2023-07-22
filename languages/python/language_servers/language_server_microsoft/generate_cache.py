
import json
import logging
import random
import string
import subprocess
import sys
from time import sleep

logging.basicConfig(format='%(message)s', level=logging.DEBUG)
log = logging.getLogger(__name__)

def send_message(stdin, msg):
    b = json.dumps(msg)
    stdin.write("Content-Length: ".encode())
    stdin.write(str(len(b)).encode())
    stdin.write("\r\n\r\n".encode())
    stdin.write(b.encode())
    stdin.flush()

def send_request(stdin, method, params):
    rid = ''.join(random.choice(string.ascii_letters + string.digits) for _ in range(10))
    d = {
        "jsonrpc": "2.0",
        "id": rid,
        "method": method,
        "params": params
    }
    log.debug("Sending request: " + json.dumps(d))
    send_message(stdin, d)

def send_notification(stdin, method, params):
    d = {
        "jsonrpc": "2.0",
        "method": method,
        "params": params
    }
    log.debug("Sending notification: " + json.dumps(d))
    send_message(stdin, d)

def receive_message(stdout):
    stdout.read(len("Content-Length: "))

    count = ''
    while True:
        maybe_digit = stdout.read(1).decode()
        if maybe_digit.isdigit():
            count += maybe_digit
        else:
            # Got the \r
            break

    count = int(count)

    stdout.read(3) # \n\r\n
    content = stdout.read(count).decode()
    log.debug("Received message: " + content)
    return json.loads(content)


path = sys.argv[1]
cache_directory = sys.argv[2]
workspace_configuration_path = sys.argv[4]

with open(sys.argv[3], 'r') as f:
    initialization_options = json.loads(f.read())
    initialization_options["cacheFolderPath"] = cache_directory

with open(sys.argv[4], 'r') as f:
    workspace_configuration = json.loads(f.read())
    workspace_configuration["python"]["analysis"]["cacheFolderPath"] = cache_directory

log.debug("Launching " + path)

with subprocess.Popen(path, stderr=None, stdout=subprocess.PIPE, stdin=subprocess.PIPE) as p:
    log.debug("Got PID " + str(p.pid))

    send_request(p.stdin, "initialize", {
        "capabilities": {},
        "initializationOptions": initialization_options
    })

    log.debug("Waiting for first message")

    receive_message(p.stdout)

    send_notification(p.stdin, "initialized", {})

    send_notification(p.stdin, "workspace/didChangeConfiguration", {
        "settings": workspace_configuration
    })

    send_notification(p.stdin, "textDocument/didOpen", {
        "textDocument": {
            "uri": "file://test.py",
            "languageId": "python",
            "version": 0,
            "text": "\n".join(["import " + x for x in ["pandas", "numpy", "scipy", "matplotlib", "requests"]])
        }
    })

    while True:
        response = receive_message(p.stdout)

        try:
            if response.get("method") != "telemetry/event":
                continue
            if response.get("params").get("EventName") == "python_language_server/analysis_complete":
                break
        except KeyError:
            continue

    log.debug("Done! Sleeping to ensure cache is written to disk")
    sleep(30)
    p.terminate()

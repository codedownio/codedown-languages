import json
import os
import requirements


def show_spec(spec):
    # Specs are tuples of (constraint, version number)
    return spec[0] + spec[1]


base_requirements = []

if os.path.exists("top-requirements.txt"):
    with open("top-requirements.txt", "r") as f:
        for req in requirements.parse(f):
            base_requirements.append(
                {
                    "name": req.name,
                    "version": str(req.specs) if len(req.specs) > 0 else None,
                }
            )

if os.path.exists("requirements.txt"):
    with open("requirements.txt", "r") as f:
        for req in requirements.parse(f):
            for baseReq in base_requirements:
                if (
                    req.name == baseReq["name"]
                    and len(req.specs) > 0
                    and baseReq["version"] == None
                ):
                    baseReq["version"] = ", ".join([show_spec(x) for x in req.specs])

# Try to get system base packages from environment variable
system_packages = []
system_packages_json_path = os.environ["SYSTEM_PACKAGES_JSON"]
if system_packages_json_path:
    with open(system_packages_json_path, 'r') as f:
        system_packages = json.loads(f.read())

print(json.dumps(system_packages + base_requirements))

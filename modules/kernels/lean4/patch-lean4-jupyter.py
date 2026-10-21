"""Build-time patches to utensil/lean4_jupyter. Run from the unpacked source root with the
wrapped `repl` binary as argv[1].

1. Spawn our `repl` instead of `lake env repl`, so a notebook needn't be a Lake project.
2. Inline the vendored stylesheets, which upstream links from lean-lang.org and jsdelivr.
   The two scripts are dropped; Alectryon already highlights server-side.
3. Drop the raw JSON `plain()` appends to every cell, but render sorried goals first --
   Alectryon shows the warning and the `--% prove N` magic, never the goal itself, so a
   straight deletion would lose it. The HTML keeps the dump behind a collapsed <details>.
"""

import pathlib
import sys

repl_bin = sys.argv[1]

VENDORED_CSS = ["alectryon.css", "pygments.css", "lean4_jupyter.css"]


def patch(path, old, new):
    p = pathlib.Path(path)
    s = p.read_text()
    if old not in s:
        raise SystemExit(f"{path}: pattern not found, upstream changed:\n{old}")
    p.write_text(s.replace(old, new, 1))


patch(
    "lean4_jupyter/repl.py",
    'pexpect.spawn("lake env repl"',
    f'pexpect.spawn("{repl_bin}"',
)
patch(
    "lean4_jupyter/repl.py",
    '\'\'\'echo \'{"cmd": "#eval Lean.versionString"}\'|repl\'\'\'',
    f'\'\'\'echo \'{{"cmd": "#eval Lean.versionString"}}\'|{repl_bin}\'\'\'',
)

patch(
    "lean4_jupyter/display.py",
    """        raw = '\\n\\nRaw input:\\n' + self.output.input.raw + '\\nRaw output:\\n' + self.output.raw
        return '\\n'.join(lines) + raw""",
    """        goals = ''.join(
            '\\nproofState %s:\\n%s' % (s['proofState'], s['goal'])
            for s in self.output.info.get('sorries', [])
        )
        return '\\n'.join(lines) + goals""",
)

style = "\n".join(
    "<style>\n%s\n</style>" % pathlib.Path("vendor", name).read_text()
    for name in VENDORED_CSS
)

display = pathlib.Path("lean4_jupyter/display.py")
source = display.read_text()
marker = "    HTML_HEADER = '''"
start = source.index(marker)
end = source.index("'''", start + len(marker)) + 3
display.write_text(source[:start] + "    HTML_HEADER = r'''\n" + style + "\n'''" + source[end:])

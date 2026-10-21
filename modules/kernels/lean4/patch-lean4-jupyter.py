"""Build-time patches to utensil/lean4_jupyter. Run from the unpacked source root with the
wrapped `repl` binary as argv[1].

Three things upstream does that don't work in a Nix environment:

1. `repl.py` spawns `lake env repl`, which only elaborates inside a Lake project directory.
   The repl we build runs from any cwd with no project at all, so point both the spawn and
   the `check()` probe straight at it. That's what lets a notebook live in a plain folder.

2. `display.py` emits an HTML header linking three stylesheets and two scripts from
   lean-lang.org and jsdelivr, so cell output only renders correctly if whoever is *viewing*
   the notebook has internet access. The same stylesheets are vendored in the repo, so
   inline them. The scripts are dropped: Alectryon's pygments lexer already highlighted the
   code server-side, and highlight.js alone is 111 KB per cell.

3. `plain()` appends the repl's JSON request and response verbatim to every cell's
   text/plain, carriage returns and all. The HTML rendering puts the same thing behind a
   collapsed <details>, so it's only noise where it can't be collapsed -- the console REPL
   and nbconvert's markdown and LaTeX exporters. Drop it there and leave the HTML alone.

   It can't be a straight deletion: a sorried goal appears *only* in that JSON. Alectryon
   renders the "declaration uses `sorry`" warning and the `--% prove N` magic but not the
   goal itself, so removing the dump without putting the goals back would lose the most
   useful line in the cell.
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

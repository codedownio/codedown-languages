#!/usr/bin/env python3

"""Render the feature matrix produced by nix/feature-matrix.nix.

    nix build .#featureMatrixJson
    scripts/render-feature-matrix.py result --format svg --mode light > docs/feature-matrix.svg
    scripts/render-feature-matrix.py result --format markdown > matrix.md

Formats:
    svg       a single graphic, languages down the side and features across the top
    markdown  one table per feature group, for places that can't take an image
"""

import argparse
import json
import sys

# Colors are the validated data-viz defaults: status green for supported, muted ink for
# not-supported (absence isn't an error, so it doesn't get a red). Nothing is encoded by
# color alone -- a supported cell also carries a check, an unsupported one a dot.
THEMES = {
    "light": {
        "surface": "#fcfcfb",
        "band": "#f0efec",
        "textPrimary": "#0b0b0b",
        "textSecondary": "#52514e",
        "muted": "#898781",
        "gridline": "#e1e0d9",
        "rowStripe": "#f6f5f2",
        "yes": "#0ca30c",
        "yesFill": "#e4f4e4",
        "no": "#c3c2b7",
    },
    "dark": {
        "surface": "#1a1a19",
        "band": "#262624",
        "textPrimary": "#ffffff",
        "textSecondary": "#c3c2b7",
        "muted": "#898781",
        "gridline": "#2c2c2a",
        "rowStripe": "#201f1e",
        "yes": "#0ca30c",
        "yesFill": "#17301a",
        "no": "#4a4a47",
    },
}

FONT = 'system-ui, -apple-system, "Segoe UI", Helvetica, Arial, sans-serif'

LABEL_WIDTH = 178
COL_WIDTH = 29
ROW_HEIGHT = 29
GROUP_BAND_HEIGHT = 22
PAD = 16
TITLE_HEIGHT = 46
LEGEND_HEIGHT = 30


def escape(text):
    return (str(text)
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace('"', "&quot;"))


def level_of(language, feature_id):
    return language["support"].get(feature_id, {}).get("level", "unknown")


def detail_of(language, feature_id):
    return language["support"].get(feature_id, {}).get("detail")


# ---------------------------------------------------------------------------
# SVG
# ---------------------------------------------------------------------------

def check_path(cx, cy):
    """A check mark centred on (cx, cy), drawn as a path so it needs no font."""
    return (f"M {cx - 4.6:.1f} {cy + 0.1:.1f} "
            f"L {cx - 1.4:.1f} {cy + 3.4:.1f} "
            f"L {cx + 4.8:.1f} {cy - 3.8:.1f}")


def render_svg(matrix, mode):
    theme = THEMES[mode]
    features = matrix["features"]
    languages = matrix["languages"]
    groups = [g for g in matrix["groups"]
              if any(f["group"] == g["id"] for f in features)]

    # Features in group order, so the group bands are contiguous.
    ordered = []
    for group in groups:
        ordered.extend([f for f in features if f["group"] == group["id"]])

    # Rotated 45 degrees, a label's extent along each axis is its length times cos(45).
    def diagonal_extent(name):
        return len(name) * 6.05 * 0.707

    header_height = int(max(diagonal_extent(f["name"]) for f in ordered)) + 14

    grid_left = PAD + LABEL_WIDTH
    grid_top = PAD + TITLE_HEIGHT + GROUP_BAND_HEIGHT + header_height
    grid_width = COL_WIDTH * len(ordered)
    grid_height = ROW_HEIGHT * len(languages)

    # The rotated labels lean up and to the right, so the rightmost one overhangs the grid.
    # Whichever column's label reaches furthest right sets the canvas width.
    rightmost = max(grid_left + i * COL_WIDTH + COL_WIDTH / 2 + 3 + diagonal_extent(f["name"])
                    for i, f in enumerate(ordered))
    width = int(max(rightmost, grid_left + grid_width)) + PAD
    height = grid_top + grid_height + LEGEND_HEIGHT + PAD

    out = []
    add = out.append

    add(f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}" role="img" '
        f'aria-label="Language feature support matrix">')
    add('  <title>codedown language feature matrix</title>')
    add(f'  <rect width="{width}" height="{height}" fill="{theme["surface"]}"/>')
    add(f'  <g font-family=\'{FONT}\'>')

    # Title
    add(f'    <text x="{PAD}" y="{PAD + 20}" font-size="16" font-weight="600" '
        f'fill="{theme["textPrimary"]}">Language feature support</text>')
    add(f'    <text x="{PAD}" y="{PAD + 38}" font-size="11" '
        f'fill="{theme["textSecondary"]}">Generated from the codedown-languages Nix modules '
        f'and live language server capabilities</text>')

    # Group bands
    band_y = PAD + TITLE_HEIGHT
    column = 0
    for group in groups:
        count = sum(1 for f in ordered if f["group"] == group["id"])
        x = grid_left + column * COL_WIDTH
        w = count * COL_WIDTH
        add(f'    <rect x="{x}" y="{band_y}" width="{w - 2}" height="{GROUP_BAND_HEIGHT - 4}" '
            f'rx="3" fill="{theme["band"]}"/>')
        add(f'    <text x="{x + w / 2 - 1:.1f}" y="{band_y + 13}" font-size="10" '
            f'font-weight="600" text-anchor="middle" fill="{theme["textSecondary"]}">'
            f'{escape(group["name"])}</text>')
        column += count

    # Rotated column headers
    label_baseline = grid_top - 8
    for i, feature in enumerate(ordered):
        x = grid_left + i * COL_WIDTH + COL_WIDTH / 2 + 3
        add(f'    <text x="{x:.1f}" y="{label_baseline}" font-size="11" '
            f'fill="{theme["textSecondary"]}" text-anchor="start" '
            f'transform="rotate(-45 {x:.1f} {label_baseline})">{escape(feature["name"])}</text>')

    # Rows
    for r, language in enumerate(languages):
        y = grid_top + r * ROW_HEIGHT
        if r % 2 == 0:
            add(f'    <rect x="{PAD}" y="{y}" width="{grid_left + grid_width - PAD}" '
                f'height="{ROW_HEIGHT}" fill="{theme["rowStripe"]}"/>')

        version = language.get("version")
        label = escape(language["displayName"])
        add(f'    <text x="{PAD + 4}" y="{y + ROW_HEIGHT / 2 + 4:.1f}" font-size="12.5" '
            f'fill="{theme["textPrimary"]}">{label}</text>')
        if version:
            add(f'    <text x="{grid_left - 8}" y="{y + ROW_HEIGHT / 2 + 4:.1f}" font-size="10" '
                f'text-anchor="end" fill="{theme["muted"]}">{escape(version)}</text>')

        for i, feature in enumerate(ordered):
            cx = grid_left + i * COL_WIDTH + COL_WIDTH / 2
            cy = y + ROW_HEIGHT / 2
            level = level_of(language, feature["id"])
            detail = detail_of(language, feature["id"])

            tooltip = f'{language["displayName"]} - {feature["name"]}: '
            tooltip += {"full": "yes", "none": "no"}.get(level, level)
            if detail:
                tooltip += f" ({detail})"

            add(f'    <g><title>{escape(tooltip)}</title>')
            if level == "full":
                add(f'      <rect x="{cx - 10.5:.1f}" y="{cy - 10.5:.1f}" width="21" height="21" '
                    f'rx="5" fill="{theme["yesFill"]}"/>')
                add(f'      <path d="{check_path(cx, cy)}" fill="none" stroke="{theme["yes"]}" '
                    f'stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>')
            elif level == "none":
                add(f'      <circle cx="{cx:.1f}" cy="{cy:.1f}" r="2.2" fill="{theme["no"]}"/>')
            else:
                add(f'      <text x="{cx:.1f}" y="{cy + 4:.1f}" font-size="11" '
                    f'text-anchor="middle" fill="{theme["muted"]}">?</text>')
            add('    </g>')

    # Column separators between groups, drawn over the rows as hairlines
    column = 0
    for group in groups[:-1]:
        column += sum(1 for f in ordered if f["group"] == group["id"])
        x = grid_left + column * COL_WIDTH
        add(f'    <line x1="{x}" y1="{grid_top}" x2="{x}" y2="{grid_top + grid_height}" '
            f'stroke="{theme["gridline"]}" stroke-width="1"/>')
    add(f'    <line x1="{grid_left}" y1="{grid_top}" x2="{grid_left}" '
        f'y2="{grid_top + grid_height}" stroke="{theme["gridline"]}" stroke-width="1"/>')

    # Legend
    legend_y = grid_top + grid_height + 19
    add(f'    <rect x="{PAD}" y="{legend_y - 9}" width="16" height="16" rx="4" '
        f'fill="{theme["yesFill"]}"/>')
    add(f'    <path d="{check_path(PAD + 8, legend_y - 1)}" fill="none" stroke="{theme["yes"]}" '
        f'stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>')
    add(f'    <text x="{PAD + 22}" y="{legend_y + 3}" font-size="11" '
        f'fill="{theme["textSecondary"]}">supported</text>')
    add(f'    <circle cx="{PAD + 96}" cy="{legend_y - 1}" r="2.2" fill="{theme["no"]}"/>')
    add(f'    <text x="{PAD + 106}" y="{legend_y + 3}" font-size="11" '
        f'fill="{theme["textSecondary"]}">not available</text>')

    has_unknown = any(level_of(l, f["id"]) not in ("full", "none")
                      for l in languages for f in ordered)
    if has_unknown:
        add(f'    <text x="{PAD + 190}" y="{legend_y + 3}" font-size="11" '
            f'text-anchor="middle" fill="{theme["muted"]}">?</text>')
        add(f'    <text x="{PAD + 200}" y="{legend_y + 3}" font-size="11" '
            f'fill="{theme["textSecondary"]}">not measured</text>')

    add('  </g>')
    add('</svg>')
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# Markdown
# ---------------------------------------------------------------------------

MARKDOWN_CELL = {"full": "✅", "none": "–", "unknown": "?"}


def render_markdown(matrix):
    features = matrix["features"]
    languages = matrix["languages"]

    out = ["✅ supported &nbsp;&nbsp; – not available &nbsp;&nbsp; ? not measured", ""]
    for group in matrix["groups"]:
        group_features = [f for f in features if f["group"] == group["id"]]
        if not group_features:
            continue

        out.append(f"### {group['name']}")
        out.append("")
        out.append("| Language | " + " | ".join(f["name"] for f in group_features) + " |")
        out.append("| --- | " + " | ".join("---" for _ in group_features) + " |")
        for language in languages:
            cells = [MARKDOWN_CELL.get(level_of(language, f["id"]), "?") for f in group_features]
            out.append(f"| {language['displayName']} | " + " | ".join(cells) + " |")
        out.append("")

    return "\n".join(out)


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", help="feature-matrix.json (or - for stdin)")
    parser.add_argument("--format", choices=["svg", "markdown"], default="svg")
    parser.add_argument("--mode", choices=["light", "dark"], default="light",
                        help="color scheme for --format svg")
    parser.add_argument("-o", "--output", help="write here instead of stdout")
    args = parser.parse_args()

    if args.input == "-":
        matrix = json.load(sys.stdin)
    else:
        with open(args.input) as handle:
            matrix = json.load(handle)

    if args.format == "svg":
        text = render_svg(matrix, args.mode)
    else:
        text = render_markdown(matrix)

    if args.output:
        with open(args.output, "w") as handle:
            handle.write(text)
    else:
        sys.stdout.write(text)


if __name__ == "__main__":
    main()

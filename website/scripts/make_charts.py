#!/usr/bin/env python3
"""
Generate self-contained, accessible, theme-aware SVG charts from data/figures.json.

Design goals (see project brief):
- Simple, static SVG: responsive via viewBox, no JavaScript required.
- Theme-aware: internal <style> reacts to prefers-color-scheme (light/dark).
- Accessible: role="img" with <title>/<desc>; series distinguished by direct
  text labels and value labels, never by color alone.
- Horizontal layout so bars/labels stay legible on narrow mobile screens.

Run from anywhere:
    python3 website/scripts/make_charts.py
Outputs SVG files into website/assets/charts/.

Numbers are read from figures.json only; this script performs no statistics.
See scripts/verify_from_source.py to re-derive figures.json from raw data.
"""
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
DATA = os.path.join(ROOT, "data", "figures.json")
OUT = os.path.join(ROOT, "assets", "charts")

# Shared palette + type, defined once so every chart matches the site.
STYLE = """
  <style>
    .chart { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
    .ink   { fill: #1b1b1a; }
    .muted { fill: #6b6b64; }
    .axis  { stroke: #d9d5cc; stroke-width: 1; }
    .grid  { stroke: #ece9e2; stroke-width: 1; }
    .bench { stroke: #1b1b1a; stroke-width: 1.5; }
    .bar-control   { fill: #6a8595; }
    .bar-treatment { fill: #a6392e; }
    .bar-stroke    { stroke: #1b1b1a; stroke-width: 1; }
    .err   { stroke: #1b1b1a; stroke-width: 1.5; }
    .val   { fill: #1b1b1a; font-weight: 600; }
    .dot-fill { fill: #a6392e; stroke: #1b1b1a; stroke-width: 1; }
    .dot-open { fill: #ffffff; stroke: #1b1b1a; stroke-width: 1.5; }
    @media (prefers-color-scheme: dark) {
      .ink, .val { fill: #ece9e2; }
      .muted { fill: #a7a49b; }
      .axis  { stroke: #4a4a45; }
      .grid  { stroke: #35352f; }
      .bench { stroke: #ece9e2; }
      .bar-control   { fill: #7fa2b4; }
      .bar-treatment { fill: #e0796b; }
      .bar-stroke    { stroke: #ece9e2; }
      .err   { stroke: #ece9e2; }
      .dot-fill { fill: #e0796b; stroke: #ece9e2; }
      .dot-open { fill: #14140f; stroke: #ece9e2; }
    }
  </style>
"""


def esc(s):
    return (str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            .replace('"', "&quot;"))


def svg_open(w, h, title, desc):
    return (
        f'<svg class="chart" viewBox="0 0 {w} {h}" width="100%" '
        f'preserveAspectRatio="xMidYMid meet" role="img" '
        f'xmlns="http://www.w3.org/2000/svg" '
        f'aria-labelledby="t{abs(hash(title))%99999} d{abs(hash(desc))%99999}">'
        f'{STYLE}'
        f'<title id="t{abs(hash(title))%99999}">{esc(title)}</title>'
        f'<desc id="d{abs(hash(desc))%99999}">{esc(desc)}</desc>'
    )


def legend(x, y, items):
    """items: list of (css_class, label)."""
    out = []
    cx = x
    for cls, label in items:
        out.append(f'<rect x="{cx}" y="{y-9}" width="13" height="13" rx="2" class="{cls} bar-stroke"/>')
        out.append(f'<text x="{cx+18}" y="{y+2}" class="ink" font-size="13">{esc(label)}</text>')
        cx += 30 + len(label) * 7.6
    return "".join(out)


def fig1(d):
    """Horizontal grouped bars: performance relative to data scientists (0)."""
    tasks = d["figure1_main_effects"]["tasks"]
    W, H = 720, 430
    L, R, T, B = 150, 96, 70, 55
    plot_w = W - L - R
    # x domain: -1 .. 0.05
    xmin, xmax = -1.0, 0.05
    def X(v): return L + (v - xmin) / (xmax - xmin) * plot_w
    s = [svg_open(W, H,
        "Effect of AI on task performance, relative to data scientists",
        "Grouped horizontal bar chart. For coding, statistics and prediction tasks, "
        "the control and treatment group mean scores are shown relative to the data-scientist "
        "benchmark at zero. Coding: control -0.63, treatment -0.14, a 49 percentage-point gain. "
        "Statistics: control -0.32, treatment -0.12, a 20-point gain. Prediction: control -0.43, "
        "treatment -0.26, a 17-point gain. All treatment groups remain below the benchmark.")]
    # gridlines + x labels
    for gv in [-1.0, -0.75, -0.5, -0.25, 0.0]:
        gx = X(gv)
        cls = "bench" if gv == 0 else "grid"
        s.append(f'<line x1="{gx:.1f}" y1="{T}" x2="{gx:.1f}" y2="{H-B}" class="{cls}"/>')
        s.append(f'<text x="{gx:.1f}" y="{H-B+20}" text-anchor="middle" class="muted" font-size="12">{gv:g}</text>')
    s.append(f'<text x="{X(0):.1f}" y="{T-28}" text-anchor="middle" class="ink" font-size="12" font-weight="600">Data scientists</text>')
    s.append(f'<text x="{X(0):.1f}" y="{T-14}" text-anchor="middle" class="muted" font-size="11">benchmark = 0</text>')
    # legend
    s.append(legend(L, T-40, [("bar-control", "Control"), ("bar-treatment", "Treatment")]))
    band = (H - T - B) / len(tasks)
    bar_h = 20
    for i, tk in enumerate(tasks):
        cy = T + band * i + band / 2
        s.append(f'<text x="{L-12}" y="{cy+1}" text-anchor="end" class="ink" font-size="14" font-weight="600">{esc(tk["task"])}</text>')
        # effect magnitude, placed to the right of the benchmark line (clear space)
        ex = X(0) + 14
        s.append(f'<text x="{ex:.1f}" y="{cy-2:.1f}" text-anchor="start" font-size="14" font-weight="700" class="bar-treatment">+{round(tk["te"]*100)} pts</text>')
        s.append(f'<text x="{ex:.1f}" y="{cy+13:.1f}" text-anchor="start" class="muted" font-size="10">gain vs. control</text>')
        # control bar (above), treatment (below)
        for j, (grp, cls) in enumerate([("control", "bar-control"), ("treatment", "bar-treatment")]):
            v = tk[grp]
            by = cy - bar_h - 2 + j * (bar_h + 4)
            x0 = X(0); x1 = X(v)
            s.append(f'<rect x="{min(x0,x1):.1f}" y="{by:.1f}" width="{abs(x1-x0):.1f}" height="{bar_h}" class="{cls} bar-stroke"/>')
            s.append(f'<text x="{x1-6:.1f}" y="{by+bar_h-5:.1f}" text-anchor="end" class="val" font-size="11.5">{v:.2f}</text>')
    s.append(f'<text x="{L+plot_w/2:.1f}" y="{H-8}" text-anchor="middle" class="muted" font-size="12.5">Performance relative to data scientists (0 = benchmark, −1 = no points)</text>')
    s.append("</svg>")
    return "".join(s)


def fig2_panel(task_name, rows, note_bayes=None):
    """One small-multiple panel for Figure 2 (a single task, three coding levels)."""
    W, H = 360, 340
    L, R, T, B = 108, 20, 44, 46
    plot_w = W - L - R
    xmin, xmax = -1.0, 0.1
    def X(v): return L + (v - xmin) / (xmax - xmin) * plot_w
    desc_bits = []
    for r in rows:
        desc_bits.append(f'{r["group"]}: control {r["control"]:.2f}, treatment {r["treatment"]:.2f}')
    s = [svg_open(W, H,
        f"{task_name}: effect by prior coding experience",
        f"Horizontal bars for the {task_name.lower()} task by prior coding experience, "
        f"relative to the data-scientist benchmark at zero. " + "; ".join(desc_bits) + ".")]
    s.append(f'<text x="{L-8}" y="{T-20}" text-anchor="start" class="ink" font-size="13.5" font-weight="700">{esc(task_name)}</text>')
    for gv in [-1.0, -0.5, 0.0]:
        gx = X(gv)
        cls = "bench" if gv == 0 else "grid"
        s.append(f'<line x1="{gx:.1f}" y1="{T}" x2="{gx:.1f}" y2="{H-B}" class="{cls}"/>')
        s.append(f'<text x="{gx:.1f}" y="{H-B+18}" text-anchor="middle" class="muted" font-size="11">{gv:g}</text>')
    band = (H - T - B) / len(rows)
    bar_h = 15
    for i, r in enumerate(rows):
        cy = T + band * i + band / 2
        s.append(f'<text x="{L-8}" y="{cy-4}" text-anchor="end" class="ink" font-size="11.5">{esc(r["group"])}</text>')
        te_txt = f'+{round(r["te"]*100)} pts'
        s.append(f'<text x="{L-8}" y="{cy+10}" text-anchor="end" class="muted" font-size="10">{te_txt}</text>')
        for j, (grp, cls) in enumerate([("control", "bar-control"), ("treatment", "bar-treatment")]):
            v = r[grp]
            by = cy - bar_h - 1 + j * (bar_h + 2)
            x0 = X(0); x1 = X(v)
            s.append(f'<rect x="{min(x0,x1):.1f}" y="{by:.1f}" width="{abs(x1-x0):.1f}" height="{bar_h}" class="{cls} bar-stroke"/>')
    s.append("</svg>")
    return "".join(s)


def fig3(d):
    """Horizontal grouped bars: share correct on post-experiment questions (no AI)."""
    qs = d["figure3_learning"]["questions"]
    W, H = 720, 520
    L, R, T, B = 300, 70, 60, 50
    plot_w = W - L - R
    def X(v): return L + v * plot_w  # 0..1
    s = [svg_open(W, H,
        "Post-experiment technical knowledge without ChatGPT",
        "Grouped horizontal bars showing the share of control and treatment workers answering "
        "five technical questions correctly, without ChatGPT. The two groups perform similarly on "
        "every question; no difference is statistically significant and a joint test finds no effect "
        "(F(5,198)=0.83, p=0.531).")]
    for gv in [0, 0.25, 0.5, 0.75, 1.0]:
        gx = X(gv)
        s.append(f'<line x1="{gx:.1f}" y1="{T}" x2="{gx:.1f}" y2="{H-B}" class="grid"/>')
        s.append(f'<text x="{gx:.1f}" y="{H-B+20}" text-anchor="middle" class="muted" font-size="12">{gv:.2f}</text>')
    s.append(f'<line x1="{X(0):.1f}" y1="{T}" x2="{X(0):.1f}" y2="{H-B}" class="axis"/>')
    s.append(legend(L, T-30, [("bar-control", "Control"), ("bar-treatment", "Treatment")]))
    band = (H - T - B) / len(qs)
    bar_h = 16
    for i, q in enumerate(qs):
        cy = T + band * i + band / 2
        # wrap label to <= ~40 chars over up to 3 lines
        label = q["label"]
        words = label.split()
        lines, cur = [], ""
        for w in words:
            if len(cur) + len(w) + 1 > 34:
                lines.append(cur); cur = w
            else:
                cur = (cur + " " + w).strip()
        if cur:
            lines.append(cur)
        lines = lines[:3]
        ly = cy - (len(lines) - 1) * 7 - 4
        for ln in lines:
            s.append(f'<text x="{L-12}" y="{ly:.1f}" text-anchor="end" class="ink" font-size="12">{esc(ln)}</text>')
            ly += 14
        for j, (grp, cls) in enumerate([("control", "bar-control"), ("treatment", "bar-treatment")]):
            v = q[grp]
            by = cy - bar_h - 2 + j * (bar_h + 4)
            s.append(f'<rect x="{X(0):.1f}" y="{by:.1f}" width="{v*plot_w:.1f}" height="{bar_h}" class="{cls} bar-stroke"/>')
            s.append(f'<text x="{X(v)+6:.1f}" y="{by+bar_h-4:.1f}" class="val" font-size="11">{v:.2f}</text>')
    s.append(f'<text x="{L+plot_w/2:.1f}" y="{H-6}" text-anchor="middle" class="muted" font-size="12.5">Share answering correctly (without ChatGPT)</text>')
    s.append("</svg>")
    return "".join(s)


def fig4(d):
    """Dot-and-interval: treatment effect on confidence that GPT-4 can answer."""
    qs = d["figure4_calibration"]["questions"]
    W, H = 720, 520
    L, R, T, B = 320, 60, 64, 52
    plot_w = W - L - R
    xmin, xmax = -6.0, 16.0
    def X(v): return L + (v - xmin) / (xmax - xmin) * plot_w
    s = [svg_open(W, H,
        "Effect of AI experience on predictions about GPT-4's abilities",
        "Dot-and-interval chart of the treatment effect, in percentage points, on workers' "
        "confidence that GPT-4 can answer each of seven questions, measured after the experiment. "
        "Filled dots mark questions GPT-4 cannot answer; open dots mark questions it can. For all "
        "four questions GPT-4 cannot answer, treated workers were significantly more confident it "
        "could, indicating worse calibration.")]
    for gv in [-5, 0, 5, 10, 15]:
        gx = X(gv)
        cls = "bench" if gv == 0 else "grid"
        s.append(f'<line x1="{gx:.1f}" y1="{T}" x2="{gx:.1f}" y2="{H-B}" class="{cls}"/>')
        s.append(f'<text x="{gx:.1f}" y="{H-B+20}" text-anchor="middle" class="muted" font-size="12">{gv:+d}</text>')
    # legend for dot meanings
    s.append(f'<circle cx="{L}" cy="{T-32}" r="6" class="dot-fill"/>')
    s.append(f'<text x="{L+12}" y="{T-28}" class="ink" font-size="12.5">GPT-4 cannot answer</text>')
    s.append(f'<circle cx="{L+170}" cy="{T-32}" r="6" class="dot-open"/>')
    s.append(f'<text x="{L+182}" y="{T-28}" class="ink" font-size="12.5">GPT-4 can answer</text>')
    band = (H - T - B) / len(qs)
    for i, q in enumerate(qs):
        cy = T + band * i + band / 2
        label = f'{q["q"]}. {q["label"]}'
        words = label.split()
        lines, cur = [], ""
        for w in words:
            if len(cur) + len(w) + 1 > 38:
                lines.append(cur); cur = w
            else:
                cur = (cur + " " + w).strip()
        if cur:
            lines.append(cur)
        lines = lines[:2]
        ly = cy - (len(lines) - 1) * 7 - 1
        for ln in lines:
            s.append(f'<text x="{L-14}" y="{ly:.1f}" text-anchor="end" class="ink" font-size="11.5">{esc(ln)}</text>')
            ly += 14
        lo, hi = q["ci"]
        s.append(f'<line x1="{X(lo):.1f}" y1="{cy:.1f}" x2="{X(hi):.1f}" y2="{cy:.1f}" class="err"/>')
        s.append(f'<line x1="{X(lo):.1f}" y1="{cy-4:.1f}" x2="{X(lo):.1f}" y2="{cy+4:.1f}" class="err"/>')
        s.append(f'<line x1="{X(hi):.1f}" y1="{cy-4:.1f}" x2="{X(hi):.1f}" y2="{cy+4:.1f}" class="err"/>')
        dot_cls = "dot-fill" if q["gpt4"] == "cannot" else "dot-open"
        s.append(f'<circle cx="{X(q["te_pp"]):.1f}" cy="{cy:.1f}" r="6" class="{dot_cls}"/>')
        star = " *" if q["significant"] else ""
        s.append(f'<text x="{X(hi)+8:.1f}" y="{cy+4:.1f}" class="val" font-size="11">{q["te_pp"]:+.1f}{star}</text>')
    s.append(f'<text x="{L+plot_w/2:.1f}" y="{H-6}" text-anchor="middle" class="muted" font-size="12.5">Treatment effect on confidence GPT-4 can answer (percentage points)</text>')
    s.append("</svg>")
    return "".join(s)


def main():
    with open(DATA) as f:
        d = json.load(f)
    os.makedirs(OUT, exist_ok=True)
    outputs = {
        "fig1_main_effects.svg": fig1(d),
        "fig3_learning.svg": fig3(d),
        "fig4_calibration.svg": fig4(d),
    }
    f2 = d["figure2_by_coding"]["tasks"]
    for task in ["Coding", "Statistics", "Prediction"]:
        outputs[f"fig2_{task.lower()}.svg"] = fig2_panel(task, f2[task])
    for name, content in outputs.items():
        with open(os.path.join(OUT, name), "w") as fh:
            fh.write('<?xml version="1.0" encoding="UTF-8"?>\n' + content + "\n")
        print("wrote", os.path.join("website/assets/charts", name))


if __name__ == "__main__":
    main()

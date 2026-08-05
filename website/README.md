# Research website — *Generative AI and the Temporary Upskilling of Knowledge Workers*

A standalone, static, public-facing website that explains the study for general
readers, management practitioners, consultants, and organizational leaders. It is
built with **plain HTML, CSS, and vanilla JavaScript** — no framework and no build
step — so it is fast, robust, accessible, and deployable as-is to GitHub Pages.

The site is **fully usable without JavaScript**: navigation, all expandable notes,
and every chart work with scripting disabled. JavaScript only adds three
conveniences (mobile-menu auto-close, active-section highlighting, copy-citation).

- **Paper (PDF):** https://www.emmawiles.com/storage/reskill.pdf
- **Repository:** https://github.com/emmawiles/Upskill_Public
- **Status:** Forthcoming in *Nature Human Behaviour*.

---

## Contents

```
website/
├── index.html                 # The entire page (semantic, single file)
├── styles.css                 # All styling; a single light editorial theme
├── main.js                    # Progressive enhancement only (site works without it)
├── data/
│   ├── figures.json           # Every displayed statistic, with sources
│   └── provenance.json        # Where each number comes from (paper + repo file)
├── assets/
│   ├── charts/                # Generated, self-contained, light-theme SVG charts
│   │   ├── fig1_main_effects.svg
│   │   ├── fig2_coding.svg  fig2_statistics.svg  fig2_prediction.svg
│   │   ├── fig3_learning.svg
│   │   └── fig4_calibration.svg
│   └── screenshots/           # Desktop + mobile deliverable screenshots
├── scripts/
│   ├── make_charts.py         # Regenerates the SVG charts from data/figures.json
│   └── verify_from_source.py  # Re-derives the numbers from the raw repo data
├── checks/
│   └── check_site.py          # Build / link / alt-text / a11y / overflow checks
├── VERIFICATION.md            # Verification table for every numerical claim
└── AUTHOR_REVIEW_REQUIRED.md  # Open items needing author sign-off
```

---

## Run it locally

No dependencies are needed just to view the site — it is static files. Serve the
`website/` directory with any static file server:

```bash
cd website
python3 -m http.server 8799
```

Then open <http://127.0.0.1:8799/index.html>. (Opening `index.html` directly via
`file://` also works, but a local server matches how GitHub Pages serves it.)

---

## Deploy to GitHub Pages

The site is plain static files, so deployment is just "serve this folder."

**Option A — publish the whole repo, site in a subfolder (recommended here):**

1. Push the repository to GitHub (this repo already lives at
   `emmawiles/Upskill_Public`).
2. In the repository, go to **Settings → Pages**.
3. Under **Build and deployment**, set **Source** to *Deploy from a branch*.
4. Choose branch `main` and folder **`/ (root)`**, then **Save**.
5. The site will be available at
   `https://emmawiles.github.io/Upskill_Public/website/`.

**Option B — serve the site at the domain root** (cleaner URL): use a GitHub Actions
workflow that publishes only the `website/` folder. Create
`.github/workflows/pages.yml`:

```yaml
name: Deploy website to Pages
on:
  push:
    branches: [main]
permissions:
  contents: read
  pages: write
  id-token: write
jobs:
  deploy:
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/configure-pages@v5
      - uses: actions/upload-pages-artifact@v3
        with:
          path: website        # publish only the website folder
      - id: deployment
        uses: actions/deploy-pages@v4
```

Then set **Settings → Pages → Source** to **GitHub Actions**. The site will be at
`https://emmawiles.github.io/Upskill_Public/`.

No `.nojekyll` file is required because the site uses no leading-underscore paths.

---

## Editing the content

**Prose** lives directly in `index.html`, organized top-to-bottom in the same order
it appears on the page (hero → overview → study → findings → workers →
organizations → academic paper → footer). Each section is a clearly commented
`<section>`. Edit the text in place; there is no template step.

**Numbers and chart data** live in `data/figures.json`. Every statistic on the site
is drawn from this file (or written inline next to a matching entry). If you change
a value there:

1. Update `data/provenance.json` and `VERIFICATION.md` to match.
2. Regenerate the charts:  `python3 scripts/make_charts.py`
3. Re-verify against the raw data:  `python3 scripts/verify_from_source.py`

**Do not** hand-edit the SVGs in `assets/charts/` — they are generated.

### Regenerating the charts

```bash
python3 scripts/make_charts.py
```

Reads `data/figures.json` and rewrites the six SVGs in `assets/charts/`. The SVGs
are self-contained and use the site's single light theme.

### Regenerating the screenshots

With the local server running and Google Chrome installed:

```bash
CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
"$CHROME" --headless --disable-gpu --hide-scrollbars \
  --blink-settings=preferredColorScheme=1 --window-size=1280,12500 \
  --screenshot=assets/screenshots/desktop-full.png http://127.0.0.1:8799/index.html
"$CHROME" --headless --disable-gpu --hide-scrollbars \
  --blink-settings=preferredColorScheme=1 --window-size=390,17300 \
  --screenshot=assets/screenshots/mobile-full.png http://127.0.0.1:8799/index.html
```

---

## Automated checks

```bash
# Structure, broken links, missing alt text, heading order, overflow risks
python3 checks/check_site.py

# Re-derive the displayed numbers from the raw repository data
#   (needs: pip install pandas statsmodels; reads ../computed_objects and ../data)
python3 scripts/verify_from_source.py
```

Accessibility was additionally audited with **axe-core 4** (WCAG 2.0 / 2.1 A & AA):
**0 violations**. All color pairs meet WCAG AA contrast. Keyboard navigation, focus
states, reduced-motion support, and semantic landmarks are built in.

---

## Design notes

- **Type & color:** a serif reading face for body copy, a system sans for UI, an
  off-white background, dark text, and a single restrained brick-red accent drawn
  from the paper's own figures. Control vs. treatment use a calm slate/red pair that
  echoes the paper while staying legible; series are always labeled directly, so no
  information is conveyed by color alone.
- **Charts:** simple, static, accessible SVGs. Each has a title, description, axis
  labels, a visible benchmark where relevant, confidence intervals where relevant, a
  plain-language `alt`, and a full data table in an adjacent expandable note. They
  scale to any width via `viewBox`, so there is never horizontal scrolling on mobile.
- **Faithfulness:** the site does not extrapolate beyond the study. In particular it
  does **not** claim the research shows whether AI can or cannot serve as a teacher;
  it reports only what the experiment measured.

#!/usr/bin/env python3
"""
Lightweight static checks for the website. No dependencies beyond the Python
standard library, so it runs anywhere (and in CI).

Checks performed:
  1. Build sanity: index.html exists and parses; referenced local assets exist.
  2. Broken internal links: every href="#id" points to an element with that id;
     every local file reference (css/js/svg/img) exists on disk.
  3. Missing image descriptions: every <img> has a non-empty alt attribute.
  4. Basic accessibility: exactly one <h1>; no heading level is skipped; the
     document has a lang attribute; a skip link is present.
  5. Obvious mobile overflow risks: flags fixed pixel widths on block content
     (a coarse heuristic; charts use viewBox and are exempt).

Exit code is non-zero if any check fails.
"""
import html.parser
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
WEB = os.path.dirname(HERE)
INDEX = os.path.join(WEB, "index.html")

errors = []
warnings = []


class Parser(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self.ids = set()
        self.hash_links = []
        self.file_refs = []
        self.imgs = []
        self.headings = []
        self.h1_count = 0
        self.lang = None
        self.has_skip = False
        self._in_h = None
        self._h_text = ""

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if "id" in a:
            self.ids.add(a["id"])
        if tag == "html" and "lang" in a:
            self.lang = a["lang"]
        if tag == "a":
            href = a.get("href", "")
            if href.startswith("#") and len(href) > 1:
                self.hash_links.append(href[1:])
            if "skip" in (a.get("class") or "") or href == "#main":
                self.has_skip = True
        for key in ("href", "src"):
            ref = a.get(key, "")
            if ref and not ref.startswith(("http://", "https://", "#", "data:", "mailto:")):
                self.file_refs.append(ref)
        if tag == "img":
            self.imgs.append(a)
        if tag in ("h1", "h2", "h3", "h4", "h5", "h6"):
            self._in_h = tag
            self._h_text = ""
            self.headings.append(int(tag[1]))
            if tag == "h1":
                self.h1_count += 1

    def handle_data(self, data):
        if self._in_h:
            self._h_text += data

    def handle_endtag(self, tag):
        if tag == self._in_h:
            self._in_h = None


def main():
    if not os.path.exists(INDEX):
        print("FAIL: index.html not found")
        return 1
    with open(INDEX, encoding="utf-8") as f:
        doc = f.read()

    p = Parser()
    p.feed(doc)

    # 1 & 2: file references exist
    for ref in sorted(set(p.file_refs)):
        path = os.path.join(WEB, ref.split("#")[0].split("?")[0])
        if not os.path.exists(path):
            errors.append(f"Broken local reference: {ref} (expected {path})")

    # 2: hash links resolve
    for h in sorted(set(p.hash_links)):
        if h not in p.ids and h != "top":
            errors.append(f"Broken in-page link: #{h} has no matching id")
    # #top is provided by <a id="top"></a>; confirm
    if "top" in p.hash_links and "top" not in p.ids:
        errors.append("#top anchor referenced but not defined")

    # 3: img alt text
    for img in p.imgs:
        alt = img.get("alt")
        if alt is None or not alt.strip():
            errors.append(f"Image missing alt text: src={img.get('src')}")
        elif len(alt.strip()) < 15:
            warnings.append(f"Image alt text is very short: src={img.get('src')} alt={alt!r}")

    # 4: accessibility basics
    if p.h1_count != 1:
        errors.append(f"Expected exactly one <h1>, found {p.h1_count}")
    if not p.lang:
        errors.append("<html> is missing a lang attribute")
    if not p.has_skip:
        errors.append("No skip link found")
    # heading order: never jump down by more than one level
    prev = 0
    for lvl in p.headings:
        if prev and lvl > prev + 1:
            warnings.append(f"Heading level jumps from h{prev} to h{lvl}")
        prev = lvl

    # 5: mobile overflow heuristic — fixed px widths in inline styles on content
    for m in re.finditer(r'style="[^"]*\bwidth:\s*(\d+)px', doc):
        w = int(m.group(1))
        if w > 500:
            warnings.append(f"Inline fixed width {w}px may overflow narrow screens")
    # width/height attrs on <img> are fine (SVG scales via CSS max-width:100%)

    # Referenced SVG charts exist and are non-empty
    charts_dir = os.path.join(WEB, "assets", "charts")
    if os.path.isdir(charts_dir):
        for svg in os.listdir(charts_dir):
            if svg.endswith(".svg") and os.path.getsize(os.path.join(charts_dir, svg)) < 200:
                errors.append(f"Chart SVG looks empty: {svg}")

    # Report
    print(f"Parsed index.html: {len(p.ids)} ids, {len(p.imgs)} images, "
          f"{len(set(p.file_refs))} local refs, {len(p.headings)} headings.")
    for w in warnings:
        print("  WARN:", w)
    if errors:
        print(f"\n{len(errors)} ERROR(S):")
        for e in errors:
            print("  -", e)
        return 1
    print("\nAll checks passed." + (f" ({len(warnings)} warning(s))" if warnings else ""))
    return 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""
Re-derive the website's chart numbers from the authoritative repository data and
compare them to data/figures.json. This is the provenance/verification step:
it proves the displayed numbers come from the paper's own data, not by hand.

Requirements: pandas, statsmodels (for the regressions). Run from anywhere; it
resolves the repository root relative to this file.

    python3 website/scripts/verify_from_source.py

- Figure 1 (main effects) and Figure 2 (by coding experience) are recomputed from
  computed_objects/experimental_data.csv.
- Figure 3 (learning) is recomputed from data/complete_data_all.dta.
- Figure 4 (calibration) values are transcribed from Appendix Table B8
  (writeup/callibration.tex) and are checked for internal consistency only
  (CI = estimate +/- 1.96*SE), since the raw belief columns are not shipped in
  the public CSV.

Exit code is non-zero if any recomputed value disagrees with figures.json beyond
tolerance, so this can run in CI.
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
WEB = os.path.dirname(HERE)
ROOT = os.path.dirname(WEB)

TOL = 0.01  # absolute tolerance on normalized scores / shares
TOL_PP = 0.6  # tolerance in percentage points for figure 4 CI reconstruction

problems = []
checks = 0


def close(a, b, tol=TOL):
    return abs(a - b) <= tol


def load_json():
    with open(os.path.join(WEB, "data", "figures.json")) as f:
        return json.load(f)


def main():
    global checks
    try:
        import pandas as pd
        import statsmodels.formula.api as smf
    except ImportError:
        print("! pandas/statsmodels not installed; skipping data recomputation.")
        print("  Install with: pip install pandas statsmodels")
        return 2

    fig = load_json()
    csv_path = os.path.join(ROOT, "computed_objects", "experimental_data.csv")
    dta_path = os.path.join(ROOT, "data", "complete_data_all.dta")

    # ---- Figure 1 ----
    if os.path.exists(csv_path):
        df = pd.read_csv(csv_path, low_memory=False)
        df["ps_score"] = -1 * df["PSMAEGradeAdjusted"]
        colmap = {"Coding": "CodingProcessGradeRelativeNorm",
                  "Statistics": "StatsOverallRelativeNorm",
                  "Prediction": "ps_score"}
        tc = df[df["treatment_arm"].isin([0, 1])].copy()
        print("\nFigure 1 — main effects (from experimental_data.csv)")
        for row in fig["figure1_main_effects"]["tasks"]:
            v = colmap[row["task"]]
            mt = df.loc[df["treatment_arm"] == 1, v].mean()
            mc = df.loc[df["treatment_arm"] == 0, v].mean()
            sub = tc.dropna(subset=[v]).copy()
            sub["treat"] = (sub["treatment_arm"] == 1).astype(int)
            m = smf.ols(f"{v} ~ treat", data=sub).fit(cov_type="HC0")
            te = m.params["treat"]
            for name, got, want in [("control", mc, row["control"]),
                                    ("treatment", mt, row["treatment"]),
                                    ("te", te, row["te"])]:
                checks += 1
                ok = close(got, want)
                if not ok:
                    problems.append(f"Fig1 {row['task']} {name}: got {got:.3f} vs json {want}")
                print(f"  {row['task']:11s} {name:10s} data={got:+.3f} json={want:+.3f} {'ok' if ok else 'MISMATCH'}")
    else:
        print("! experimental_data.csv not found; skipping Figures 1 & 2.")

    # ---- Figure 2 ----
    if os.path.exists(csv_path):
        print("\nFigure 2 — by coding experience (from experimental_data.csv)")
        codes = {"Never coded": "No coding experience", "Basic coding": "Coding basics",
                 "Competent coder": "Competent coder"}
        for task, col in colmap.items():
            for entry in fig["figure2_by_coding"]["tasks"][task]:
                grp = codes[entry["group"]]
                sub = df[df["know_code"] == grp].dropna(subset=[col]).copy()
                mc = sub.loc[sub["treatment"] == 0, col].mean()
                m = smf.ols(f"{col} ~ treatment", data=sub).fit()
                te = m.params["treatment"]
                for name, got, want in [("control", mc, entry["control"]),
                                        ("te", te, entry["te"])]:
                    checks += 1
                    ok = close(got, want)
                    if not ok:
                        problems.append(f"Fig2 {task}/{entry['group']} {name}: got {got:.3f} vs json {want}")
                print(f"  {task:11s} {entry['group']:16s} te data={te:+.3f} json={entry['te']:+.3f} "
                      f"{'ok' if close(te, entry['te']) else 'MISMATCH'}")

    # ---- Figure 3 ----
    if os.path.exists(dta_path):
        print("\nFigure 3 — post-experiment knowledge (from complete_data_all.dta)")
        dl = pd.read_stata(dta_path, convert_categoricals=False)
        dl["treatment"] = dl["Group"].map({"Test": 1, "Control": 0})
        dl = dl[dl["treatment"].notna()].copy()
        cols = ["DSKnowledgeQ1Correct", "DSKnowledgeQ2Correct", "DSKnowledgeQ3Correct",
                "DSKnowledgeQ4Correct", "DSKnowledgeQ5Correct"]
        for i, q in enumerate(fig["figure3_learning"]["questions"]):
            col = cols[i]
            sub = dl.dropna(subset=[col])
            mc = sub.loc[sub["treatment"] == 0, col].mean()
            m = smf.ols(f"{col} ~ treatment", data=sub).fit(cov_type="HC0")
            te = m.params["treatment"] * 100
            checks += 2
            ok_c = close(round(mc, 2), q["control"], 0.02)
            ok_t = close(round(te, 1), q["te_pp"], TOL_PP)
            if not ok_c:
                problems.append(f"Fig3 {q['q']} control: got {mc:.3f} vs json {q['control']}")
            if not ok_t:
                problems.append(f"Fig3 {q['q']} te_pp: got {te:.1f} vs json {q['te_pp']}")
            print(f"  {q['q']} control data={mc:.3f} json={q['control']} | te data={te:+.1f}pp json={q['te_pp']:+.1f}pp "
                  f"{'ok' if ok_c and ok_t else 'MISMATCH'}")
    else:
        print("! complete_data_all.dta not found; skipping Figure 3.")

    # ---- Figure 4: internal consistency of CI = est +/- 1.96*SE ----
    print("\nFigure 4 — calibration (CI reconstruction check)")
    for q in fig["figure4_calibration"]["questions"]:
        lo, hi = q["ci"]
        exp_lo = q["te_pp"] - 1.96 * q["se"]
        exp_hi = q["te_pp"] + 1.96 * q["se"]
        checks += 1
        ok = close(lo, exp_lo, TOL_PP) and close(hi, exp_hi, TOL_PP)
        if not ok:
            problems.append(f"Fig4 {q['q']} CI: json [{lo},{hi}] vs est+/-1.96se [{exp_lo:.1f},{exp_hi:.1f}]")
        print(f"  {q['q']} te={q['te_pp']:+.1f} se={q['se']} CI=[{lo},{hi}] "
              f"expect=[{exp_lo:.1f},{exp_hi:.1f}] {'ok' if ok else 'MISMATCH'}")

    print(f"\n{checks} checks run.")
    if problems:
        print(f"\n{len(problems)} MISMATCH(es):")
        for p in problems:
            print("  -", p)
        return 1
    print("All checks passed: website numbers match the source data.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

# ShinyAESC Enhancement Plan

## Guiding Principle
The app's purpose is to produce **practically useful, lay consumer-friendly, and interpretable effect sizes**. Improvements should prioritize accessibility and communication over statistical rigor for its own sake.

---

## Confirmed Features

### 1. Hedges' g Correction Factor
**Status:** Approved
**Effort:** Low
**Description:** Add the J correction factor to the existing Hedges' g calculation.
```r
J <- 1 - (3 / (4 * (n1 + n2) - 9))
hedges_g <- J * cohens_d
```

### 2. Effect Size Converter (Hypothetical Mode)
**Status:** Approved
**Effort:** Medium
**Description:** A standalone calculator mode where users input a single effect size (r, d, etc.) without raw data and get theoretical conversions to all other metrics.

**Features:**
- Input: correlation coefficient OR Cohen's d OR other common metric
- Output: Theoretical CLES, BESD, expectancy proportions, etc.
- Formulas for conversions:
  - r → d: `d = 2r / sqrt(1 - r²)`
  - d → CLES: `pnorm(d / sqrt(2))`
  - r → BESD: `0.5 + r/2` and `0.5 - r/2`

**Use case:** Researchers who have summary statistics from published papers but no raw data.

### 3. Icon Array / Pictograph for CLES
**Status:** Approved
**Effort:** Medium
**Description:** Visual representation of CLES using icon arrays (e.g., 100 person icons, colored to show probability). Research shows this format improves probability comprehension for lay audiences.

**Example output:** "Out of 100 random pairs, approximately 62 times the high-scoring person will outperform the low-scoring person" with corresponding visual.

---

## Features Requiring Design Decisions

### 4. Confidence Intervals / Uncertainty Quantification
**Status:** Needs decision
**Question:** What form of uncertainty should be displayed?

**Options:**
| Option | Pros | Cons |
|--------|------|------|
| **Frequentist CI** | Familiar to academics, standard | May confuse lay users, interpretation tricky |
| **Standard Error** | Simple, common | Less intuitive than CI |
| **Bayesian Credible Interval** | More intuitive interpretation ("95% probability the true value is in this range") | Requires prior specification, less familiar |
| **None (point estimate only)** | Simplest, aligns with translational focus | Loses uncertainty information |
| **Optional toggle** | User choice | Adds complexity |

**Recommendation:** Consider a simple "uncertainty range" label rather than technical terms. Or show uncertainty only in the detailed/advanced view, not the summary.

**Decision needed:** Which approach best serves lay consumers?

---

### 5. Theoretical vs. Empirical Effect Size Toggle
**Status:** Needs clarification
**Question:** What exactly should "theoretical" mean?

**Option A: Both require raw data**
- Empirical: Actual proportions computed directly from the data
- Theoretical: Proportions derived from the sample correlation (assuming bivariate normality)
- Purpose: Show users how the two can differ, especially with non-linear relationships

**Option B: Theoretical requires only effect size input (no data)**
- This overlaps with the "Effect Size Converter" feature (#2)
- Could be merged into that feature

**Recommendation:** If Option A, implement as a toggle within the main analysis view. If Option B, merge with the Effect Size Converter.

**Decision needed:** Which interpretation do you prefer?

---

### 6. Multiple Predictors / Incremental Validity
**Status:** Deferred - needs more thought
**Complexity:** High

**Challenges:**
- Formula complexity (semi-partial correlations, ΔR²)
- Interpretation difficult for lay users ("what does 'variance explained beyond X' mean?")
- UI complexity (selecting multiple predictors, ordering)

**Possible simplified approach:**
- Only allow 2 predictors
- Show: "Predictor 1 alone: r = .30, Predictor 2 adds: Δr = .08"
- CLES interpretation: "Adding SAT to high school GPA increases prediction accuracy from 60% to 64%"

**Decision needed:** Is the simplified 2-predictor approach worth pursuing, or defer entirely?

---

### 7. Plain Language Report Generator
**Status:** Interested, exploring AI option
**Question:** Should this be AI-generated or template-based?

**Option A: Template-based (deterministic)**
```
"The correlation between {X} and {Y} was {r_strength} (r = {r}).
Using a cutoff at the {percentile}th percentile of {X}, individuals
in the upper group had a {CLES}% probability of scoring higher on
{Y} than those in the lower group."
```
- Pros: Predictable, no API costs, works offline
- Cons: Less flexible, may sound robotic

**Option B: AI-generated (LLM API)**
- Pros: More natural language, can adapt to context
- Cons: Requires API key, costs, latency, potential inconsistency

**Option C: Hybrid**
- Template for core statistics
- Optional AI enhancement for interpretation/context

**Recommendation:** Start with template-based (Option A), add AI as optional enhancement later.

**Decision needed:** Template-based acceptable for v1?

---

## Additional Consumer-Friendly Effect Sizes to Consider

### High Priority (intuitive, visual)
- [ ] **Icon array for CLES** (approved above)
- [ ] **Cohen's U3** - "X% of the high group exceeds the typical person in the low group"
- [ ] **Percent overlap** - Simple number from density plot

### Medium Priority
- [ ] **Number Needed to Treat (NNT)** - "For every N people, one additional person succeeds"
- [ ] **Relative Risk** - "High scorers are X times more likely to succeed"

### Lower Priority (more technical)
- [ ] **Probability of Superiority (A statistic)** - Non-parametric CLES
- [ ] **Odds Ratio** - Common but less intuitive

---

## Implementation Phases

### Phase 1: Quick Wins
1. Hedges' g correction
2. Effect size converter (hypothetical mode)
3. Template-based plain language summary

### Phase 2: Visual Enhancements
4. Icon array for CLES
5. Cohen's U3 display
6. Percent overlap from density plot

### Phase 3: Design Decisions Required
7. Uncertainty quantification approach
8. Theoretical vs. empirical toggle
9. AI-enhanced language generation (optional)

### Phase 4: Future Consideration
10. Multiple predictors (if simplified approach viable)
11. Additional effect sizes (NNT, RR, etc.)

---

## Open Questions

1. **Uncertainty display:** CI vs SE vs Bayesian vs none vs optional?
2. **Theoretical toggle:** Both from data (Option A) or merge with converter (Option B)?
3. **Multiple predictors:** Worth pursuing simplified version or defer?
4. **Plain language:** Template-based acceptable for initial version?

---

## References

- Zhang, D. C. (2018). Utility of alternative effect size statistics and the development of a web-based calculator: Shiny-AESC. *Frontiers in Psychology, 9*, 1221.
- McGraw, K. O., & Wong, S. P. (1992). A common language effect size statistic. *Psychological Bulletin, 111*, 361-365.
- Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose display of magnitude of experimental effect. *Journal of Educational Psychology, 74*, 166-169.

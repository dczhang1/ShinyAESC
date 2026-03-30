# ESCAPE Landing Page — Design Recommendations

> Reviewed: 2026-03-30
> Scope: `app.R` (landing_page_ui), `www/css/landing.css`, `www/js/main.js`

---

## Executive Summary

The current landing page is **clean and functional but safe**. It communicates features correctly but doesn't lead with the emotional hook that converts a skeptical researcher into a user. The core problem: a visitor doesn't immediately *feel* the translation that ESCAPE performs — they're told about it in prose. Every recommendation below pushes toward making the transformation visceral and immediate.

Three highest-leverage changes (do these first):
1. Rewrite the hero around a concrete before/after transformation
2. Kill the carousel — lay the use cases flat
3. Use the Fraunces serif font you're already loading but not using

---

## 1. Hero Section

### Copy

**Current headline:** "Translate Statistics into Everyday Impact"
This is accurate but generic — it could describe any data communication tool.

**Recommended alternatives (ranked):**

> A. `Your r = 0.42 means 71% of patients improve. Show that.`
> B. `Stop reporting r = 0.35. Start saying: "67% of top scorers outperform."`
> C. `Effect sizes are abstract. Their meaning isn't.`

Option A is strongest — it opens with the user's work (`r = 0.42`), delivers the transformation immediately, and ends with a clear imperative.

**Current subtitle** (app.R line 59–61) is too long. Tighten it:

> *Before:* "Traditional effect sizes like correlations are hard to understand. ESCAPE transforms abstract coefficients into intuitive probabilities, success rates, and plain-language metrics that anyone can use to make informed decisions."
>
> *After:* "Correlations are hard to explain to anyone who isn't a statistician. ESCAPE converts r, d, and g into probabilities, success rates, and icon arrays that communicate to non-technical audiences."

**Trust signal** — add one line below the CTAs:
```
No account needed · Your data never leaves your browser · Works with CSV, Excel, SPSS, SAS
```

### Hero Visual

The current icon array is clever but **hidden at ≤1024px** (`display: none` in the media query), and it lacks context — a visitor doesn't know what 69 purple icons out of 100 means without reading the surrounding copy.

**Recommended replacement: a "Before / After" transformation card**

Split the hero visual card into two panels:

```
┌─────────────────────────────────────────┐
│  BEFORE                  AFTER           │
│  ─────────────────────   ──────────────  │
│  r = 0.42                71 out of 100   │
│  p < .001         →      [icon array]    │
│  Cohen's d = 0.90        respond better  │
└─────────────────────────────────────────┘
```

- Left side: monospace font, muted color, styled like a stats output block
- Right side: icon array (mini, 10×10) + plain language caption
- A subtle animated arrow or chevron between the two states
- This is the app's entire value prop in one glance

If keeping the current icon array, at minimum add a caption:
```
"Out of 100 applicants, 69 high-scorers outperform their peers"
```
...and extend the animation delays. Currently all 100 icons pop in between 0.05s–0.32s — extend to 0.05s–1.2s for a genuine wave-fill effect that builds anticipation.

### CTA Buttons

In `app.R` both buttons are bare `actionButton()` calls with no class — Bootstrap renders them both as `.btn-default`. Make the hierarchy explicit:

```r
# Primary
actionButton("try_sample", "Try with Sample Data", class = "btn-primary")

# Secondary
actionButton("open_guided_upload", "Upload Your Data", class = "btn-outline-primary")
```

The primary button should be visually dominant. As-is, both buttons compete equally.

### App Name Badge

The badge text "ESCAPE — Effect Size Calculator for Practical Effects" is too long for a pill badge. Split it:

```
[ESCAPE]  ←  just the acronym in the badge
Effect Size Calculator for Practical Effects  ←  as a separate line above the h1
```

This makes the badge punchy and lets the full name breathe as a subtitle.

---

## 2. Typography — Use Fraunces

`Fraunces` is imported in the CSS but **not applied anywhere on the landing page**. This is the single biggest missed opportunity.

Fraunces is a variable optical-size serif with genuine personality — academic, warm, authoritative. It would immediately distinguish ESCAPE from generic SaaS tools and reinforce the research-context credibility.

**Recommended applications:**

```css
/* Hero title */
.hero-title {
  font-family: var(--font-serif);  /* Fraunces */
  font-size: 3.75rem;
  font-weight: 700;
  font-variation-settings: 'opsz' 144, 'SOFT' 50;
}

/* Section titles */
.section-title {
  font-family: var(--font-serif);
  font-weight: 600;
  font-variation-settings: 'opsz' 72;
}

/* The "AFTER" stat in the transformation card */
.transformation-stat {
  font-family: var(--font-serif);
  font-size: 2.5rem;
  font-weight: 800;
}
```

Keep Sora for body copy, nav, labels, and UI elements. The serif/sans contrast adds editorial depth.

---

## 3. Page Structure — Reorder Sections

**Current order:**
1. Hero
2. Features (6 cards)
3. Use Cases (carousel)
4. Sample Data Preview
5. Footer

**Problem:** Features come before the user understands *why they need them*. The carousel buries the strongest proof points. The sample data section is the most convincing element but it's last.

**Recommended order:**
1. Hero
2. **The Problem** — "Why r = 0.35 isn't enough" (new section, ~200px tall)
3. **How It Works** — 3-step workflow (new section)
4. Sample Data Preview ← move up, it's your best demo
5. Use Cases — flat layout (not carousel)
6. Features (validation, not introduction)
7. Footer

---

## 4. New Section: "The Problem"

Add a brief section between the hero and features that articulates the pain. It should be fast to read — a single callout, not a long explanation.

**Design concept:**

```
┌──────────────────────────────────────────────────────────────┐
│  The problem with reporting statistics                        │
│                                                              │
│  "r = 0.35, p < .001"    → means nothing to a hiring manager │
│  "Cohen's d = 0.74"      → means nothing to a hospital board │
│  "R² = 0.12"             → means nothing to a school parent  │
│                                                              │
│  ESCAPE translates these numbers into language that           │
│  drives decisions.                                           │
└──────────────────────────────────────────────────────────────┘
```

- Monospace font for the "before" values (reinforce that these are raw outputs)
- Light purple left-border accent
- Background: the existing `rgba(139, 110, 240, 0.04)` — very subtle

This section speaks directly to the frustration every researcher in the target audience has lived.

---

## 5. New Section: "How It Works" (3 Steps)

Insert before or after the Sample Data section:

```
[1] Upload         [2] Analyze          [3] Communicate
Drop in your   →  Select variables  →  Get plain-language
CSV / Excel /     and effect size       reports with icon
SPSS / SAS        metrics               arrays and charts
```

- Three columns on desktop, stacked on mobile
- Large step numbers in Fraunces at low opacity (decorative)
- Connecting arrows or a progress line between steps
- Emphasize step 3 slightly (it's the payoff)

---

## 6. Use Cases — Replace the Carousel

The carousel is the weakest design decision on the page. It hides 5 of 6 use cases at any time. The copy inside each card contains the most compelling proof points on the entire landing page:

- "67% of high-scoring applicants outperform"
- "71% chance of better outcomes"
- "2.3× more likely to graduate on time"
- "78% of individuals correctly identified"

These numbers are GOLD. They're being buried in a rotating card.

**Option A (preferred): Horizontal scroll row**

Show 3 cards on desktop, scroll horizontally on mobile. All cards are partially visible, inviting exploration.

```css
.use-cases-track {
  display: flex;
  gap: var(--space-6);
  overflow-x: auto;
  scroll-snap-type: x mandatory;
  padding-bottom: var(--space-4);
  -webkit-overflow-scrolling: touch;
}

.use-case-card {
  flex: 0 0 340px;
  scroll-snap-align: start;
}
```

**Option B: 2×3 card grid**

All 6 visible simultaneously. Remove the images to save vertical space — the copy is stronger than the stock photos.

**If keeping the carousel:**
- Remove the stock photos entirely to give the text more room
- Surface the key statistic as a large pull-quote at the top of each card in Fraunces
- Slow the auto-advance to 8 seconds (5 is not enough to read the descriptions)
- Add swipe support for touch devices (currently arrow-key-only keyboard nav exists, add `touchstart`/`touchend` handlers)

**Section title fix:**

> Current: "Real-World Use Cases"
> Better: "What researchers are saying instead of r ="

---

## 7. Sample Data Section — Strengthen the Insight

**Current insight copy** (app.R line 239–241):
> "Students scoring in the top 25% on SAT have a **22% chance** of achieving above-average GPA (3.5+), compared to **4% for those in the bottom 75%**."

This is confusing. 22% still sounds low. The story is the **ratio** — they're 5.5× more likely.

**Rewrite:**
> "Students in the top 25% of SAT scores are **5.5× more likely** to achieve a 3.5+ GPA — that's the kind of statement that gets a hiring committee's attention."

The table header context is missing. The values (127, 122, 116...) look like an arbitrary 3-digit scale. Add a note:
```html
<caption>Sample dataset: SAT scores (scaled, n = 1,000) vs. College GPA</caption>
```

**Layout improvement:** The insight panel is currently equal-width to the table. Make it 60% width — it's more important than the raw data table, and the embedded plot deserves space.

**Consider adding a second stat card** showing the Cohen's d → plain language translation:
> "Cohen's d = 0.58 → Students who prep have a **66% chance** of outperforming those who don't"

This demonstrates multiple output modes and reinforces the app's range.

---

## 8. Features Section

### Section Title

> Current: "Everything you need for effect size analysis"
> Better: "From raw numbers to boardroom-ready insight"
> Or: "The complete toolkit for effect size communication"

### Individual Card Improvements

Add a one-line **micro-example** below each feature description in a lighter monospace style:

| Feature | Add micro-example |
|---------|------------------|
| Expectancy Charts | `r = 0.35 → "67 out of 100 top scorers succeed"` |
| Multiple Effect Sizes | `Cohen's d, Hedges' g, CLES, BESD — all in one view` |
| Export Reports | `One-click HTML report, ready to email to stakeholders` |
| Privacy First | `Runs entirely in your browser — no server, no uploads` |

### Visual Differentiation

All 6 feature icons use identical purple-to-gold radial gradient backgrounds. Consider assigning each a distinct accent color matching the use case palette already defined in the carousel:

- Expectancy Charts → blue
- Multiple Effect Sizes → purple (primary, as-is)
- Bridge Statistics → teal
- Export Reports → green
- Multiple Formats → orange
- Privacy First → green with a stronger emphasis (this is a genuine differentiator)

### Privacy First Card

This is your most differentiating feature for a research audience concerned about IRB and data privacy. Don't bury it as card #6. Either:
- Move it to card #1 or #2
- Make it span full width at the bottom with a distinct green background
- Add a shield icon with a checkmark glyph instead of the generic `shield-check`

---

## 9. Animation Refinements

### Icon Array Wave Effect

Current animation delays (`0.05s` to `0.32s` for 10 columns) make all 100 icons pop in almost simultaneously. Extend the range to create a genuine wave:

```r
# In landing_page_ui(), the delay class determines the animation delay
# Current: delay-1 through delay-10 map to 0.05s → 0.32s
# Recommended: keep 10 columns but map row-by-row for a wave
div(
  class = paste0("array-icon success-icon row-", ceiling(i/10), "-col-", ((i - 1) %% 10) + 1),
  ...
)
```

```css
/* CSS wave by row */
.row-1 { animation-delay: calc(0.05s * var(--col)); }
.row-2 { animation-delay: calc(0.05s * var(--col) + 0.2s); }
/* etc. */
```

Or in CSS, use a simple formula: `animation-delay: calc((i * 0.02s))` where `i` is the icon index (0–99), giving a 0s → 2s range for a clear left-to-right, top-to-bottom fill effect.

### Scroll Animations

The IntersectionObserver in `main.js` triggers fade-ins but the threshold/timing is not visible in the current code. Ensure:
- Features cards stagger by `0.08s` increments (not all at once)
- Use case cards animate in as they enter the viewport
- Sample section numbers `countUp` when the section scrolls into view

### Carousel Transition

Current transition is `opacity 0.5s ease` only. Add a subtle slide:

```css
.carousel-slide {
  transform: translateX(20px);
  opacity: 0;
  transition: opacity 0.4s ease, transform 0.4s ease, visibility 0.4s ease;
}

.carousel-slide.active {
  transform: translateX(0);
  opacity: 1;
}
```

---

## 10. Background & Atmosphere

**Current:** `radial-gradient(circle at top left, #fdf7ff, #fffdf5)` — nearly invisible, indistinguishable from white.

The background should *suggest data* without being distracting. Options:

**Option A: Dot grid** (subtle, data-adjacent)
```css
.landing-page {
  background-image: radial-gradient(rgba(139, 110, 240, 0.07) 1px, transparent 1px);
  background-size: 24px 24px;
  background-color: #fdfcff;
}
```

**Option B: Noise texture** (premium feel)
```css
.landing-page::before {
  content: '';
  position: fixed;
  inset: 0;
  background-image: url("data:image/svg+xml,..."); /* SVG noise */
  opacity: 0.03;
  pointer-events: none;
  z-index: 0;
}
```

**Option C: Diagonal fade zones** — subtle color regions that correspond to sections (hero is light purple, features is neutral, use cases picks up gold tones)

---

## 11. Footer — Expand Minimally

Current footer is a single line "Built with ❤️ using R Shiny." For an academic audience, add:

- A brief methodological note: "Effect size calculations follow Borenstein et al. (2009) and Hunter & Schmidt (2004)"
- Version/last-updated date
- Link to GitHub repository (if open source)
- Contact or feedback link

This builds credibility with the research audience who cares about methodological transparency.

---

## 12. Responsive: Keep the Hero Visual on Tablet

Currently `.hero-visual { display: none }` at ≤1024px. This removes the most distinctive element on tablets — a common screen size for researchers at conferences or reading on iPad.

Instead of hiding it, collapse the hero to a single column and **place the transformation card below the CTAs**, full-width, at reduced height:

```css
@media (max-width: 1024px) {
  .hero-section {
    grid-template-columns: 1fr;
  }

  .hero-visual {
    display: flex; /* keep it visible */
  }

  .icon-array-card {
    max-width: 100%;
    margin-top: var(--space-8);
  }
}
```

---

## Priority Stack

| Priority | Change | Effort | Impact |
|----------|--------|--------|--------|
| 1 | Rewrite hero headline to show a concrete transformation | Low | High |
| 2 | Add trust signal line below CTAs | Low | Medium |
| 3 | Apply Fraunces to hero title and section headings | Low | High |
| 4 | Extend icon array animation wave (0.05s → 1.2s range) | Low | Medium |
| 5 | Fix sample data insight copy (lead with 5.5× ratio) | Low | High |
| 6 | Replace carousel with horizontal scroll or flat grid | Medium | High |
| 7 | Add "The Problem" interstitial section | Medium | High |
| 8 | Add "How It Works" 3-step section | Medium | Medium |
| 9 | Make CTAs visually hierarchical (primary vs secondary) | Low | Medium |
| 10 | Differentiate feature card icon colors | Low | Low |
| 11 | Add dot-grid background to hero | Low | Medium |
| 12 | Expand footer with methodology references | Low | Medium |
| 13 | Keep hero visual visible on tablet | Low | Medium |
| 14 | Add micro-examples to feature cards | Medium | Medium |
| 15 | Add "Before / After" transformation card to hero | High | High |

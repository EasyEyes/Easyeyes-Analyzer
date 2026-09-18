# Plots page

Summary of plots shown on the **Plots** tab, in page order. Titles match plot subtitles / download filenames where possible. Some plots appear only when the relevant data exist.

## Correlation matrix

- **Correlations** — Heatmap of Pearson correlations between participant-level measures (reading, RSVP, crowding, acuity, age, etc.).
- **N (Non-missing Pairs)** — Same layout; each cell is the number of participants used for that correlation.

## Histograms

### Main histogram grid

- **Histogram of mean auditoryCrowdingMelodyDb** — Distribution of per-participant mean auditory crowding melody level (dB).
- **Histogram of foveal acuity** — Distribution of log foveal acuity (deg).
- **Histogram of peripheral acuity** — Distribution of log peripheral acuity (deg; geometric mean of left and right).
- **Histogram of foveal crowding** — Distribution of log foveal crowding threshold (deg).
- **Histogram of peripheral crowding** — Distribution of log peripheral crowding (deg).
- **Histogram of RSVP reading speed** — Distribution of log RSVP reading speed (w/min).
- **Histogram of reading speed** — Distribution of log ordinary reading speed (w/min).
- **Histogram of repeated-letter crowding** — Distribution of log repeated-letter crowding (deg).
- **Histogram of age** — Distribution of participant age.
- **Histogram of grades** — Count of participants by grade.
- **Histogram of proportion correct (by condition)** — Per quest condition: distribution of proportion correct.
- **Histogram of comprehension accuracy** — Reading comprehension question accuracy (optional split by condition).
- **Device / display histograms** — When variation exists: `screenWidthPx`, `screenWidthCm`, `deviceMemoryGB`, `devicePixelRatio`, `cores`.
- **Histogram of spacingMinDeg** — Distribution of minimum spacing threshold.

### Stacked by grade

Shown when more than one grade is present (and the matching task exists):

- **Histogram of RSVP reading stacked by grade** — Log RSVP reading speed, faceted by grade.
- **Histogram of peripheral crowding stacked by grade** — Log peripheral crowding, faceted by grade.
- **Histogram of foveal acuity stacked by grade** — Log foveal acuity, faceted by grade.
- **Histogram of foveal crowding stacked by grade** — Log foveal crowding, faceted by grade.
- **Histogram of foveal repeated-letter crowding stacked by grade** — Log repeated-letter crowding, faceted by grade.
- **Histogram of peripheral acuity stacked by grade** — Log peripheral acuity, faceted by grade.

## Violin plots

- **Reading Speed by Font** — Ordinary reading speed (w/min) vs font.
- **RSVP Reading Speed by Font** — RSVP reading speed vs font.
- **Crowding Threshold by Font** — Crowding distance (deg) vs font.
- **Acuity vs. font** — Acuity (deg) vs font.
- **Beauty Rating by Font** — Beauty rating vs font.
- **Comfort Rating by Font** — Comfort rating vs font.
- **Familiarity by Font** — Familiarity rating vs font.

## Font comparison plots

- **Reading** — Geometric mean ordinary reading speed by font (± SE).
- **RSVP** — Geometric mean RSVP reading speed by font (± SE).
- **Crowding** — Geometric mean crowding distance by font (± SE).
- **Comfort** — Mean comfort rating by font (± SE).
- **Beauty** — Mean beauty rating by font (± SE).
- **Acuity** — Geometric mean acuity by font (± SE).
- **Familiarity** — Mean familiarity rating by font (± SE).

## Scatter diagrams

- **auditoryCrowdingMelodyDb vs crowding threshold** — Auditory melody crowding (dB) vs peripheral crowding (deg).
- **Reading retest vs test** — Ordinary reading test–retest.
- **Peripheral crowding retest vs test** — Peripheral crowding test–retest.
- **Peripheral acuity retest vs test** — Peripheral acuity test–retest.
- **Beauty retest vs test** — Beauty rating test–retest.
- **Comfort retest vs test** — Comfort rating test–retest.
- **Foveal acuity vs foveal crowding (by grade)** — Foveal crowding vs foveal acuity.
- **Peripheral acuity vs foveal crowding (by grade)** — Foveal crowding vs peripheral acuity.
- **Peripheral acuity vs foveal acuity (by grade)** — Foveal vs peripheral acuity.
- **Peripheral crowding vs foveal crowding (by grade)** — Foveal vs peripheral crowding.
- **Peripheral acuity vs peripheral crowding (by grade)** — Peripheral crowding vs peripheral acuity.
- **Peripheral acuity vs peripheral crowding (by font)** — Same relationship, colored by font.
- **Peripheral crowding left vs right** — Left vs right peripheral crowding.
- **Reading / RSVP vs foveal crowding** — Foveal crowding vs ordinary and RSVP reading speed.
- **Reading / RSVP vs peripheral crowding** — Peripheral crowding vs ordinary and RSVP reading speed.
- **Ordinary and RSVP reading vs acuity** — Acuity vs ordinary and RSVP reading speed.
- **Reading vs RSVP reading** — RSVP speed vs ordinary reading speed.
- **Repeated-letter crowding vs foveal crowding (by grade)** — Foveal vs repeated-letter crowding.
- **Crowding vs duration** — Stimulus duration vs mean crowding (aggregate).
- **Crowding vs duration by side** — Duration vs crowding, left vs right.
- **Crowding vs duration by participant** — Duration vs crowding, colored by participant.
- **badLatenessTrials vs deviceMemoryGB** — Late trials vs device memory, by participant.
- **Foveal crowding vs spacingMinDeg** — Minimum spacing vs foveal crowding.
- **Comfort vs crowding** — Crowding vs comfort rating.
- **Beauty vs crowding** — Crowding vs beauty rating.
- **Beauty vs comfort** — Comfort vs beauty ratings.
- **Familiarity vs crowding** — Crowding vs familiarity rating.

## RSVP plots

- **RSVP vs peripheral crowding (by grade)** — Peripheral crowding vs RSVP speed.
- **RSVP vs peripheral crowding (by font)** — Same, colored by font.
- **Residual RSVP vs residual peripheral crowding** — Age-residualized crowding vs RSVP.
- **RSVP vs foveal crowding (by grade)** — Foveal crowding vs RSVP speed.
- **RSVP vs foveal acuity (by grade)** — Foveal acuity vs RSVP speed.
- **RSVP vs peripheral acuity (by font)** — Peripheral acuity vs RSVP speed.
- **RSVP vs peripheral acuity (by grade)** — Peripheral acuity vs RSVP speed.
- **RSVP vs repeated-letter crowding (by grade)** — Repeated-letter crowding vs RSVP speed.

## Ordinary reading plots

- **Ordinary reading vs peripheral crowding (by font / by grade)** — Peripheral crowding vs ordinary reading speed.
- **Ordinary reading vs foveal crowding (by font / by grade)** — Foveal crowding vs ordinary reading speed.
- **Ordinary reading vs foveal acuity (by font / by grade)** — Foveal acuity vs ordinary reading speed.
- **Ordinary reading vs peripheral acuity (by font / by grade)** — Peripheral acuity vs ordinary reading speed.
- **Reading vs repeated-letter crowding (by grade)** — Repeated-letter crowding vs ordinary reading speed.

## Font plots

Font-level aggregates (when reading/RSVP and peripheral crowding exist):

- **Reading vs peripheral crowding (ordinary + RSVP)** — Mean peripheral crowding vs reading speed by font; shapes distinguish ordinary vs RSVP.
- **Ordinary reading vs peripheral crowding** — Font aggregates for ordinary reading only.
- **RSVP vs peripheral crowding** — Font aggregates for RSVP only.

## Age plots

- **Peripheral crowding vs age (by grade)** — Age vs peripheral crowding.
- **Peripheral crowding (L/R geo mean) vs age (by grade)** — Age vs averaged peripheral crowding.
- **Foveal crowding vs age (by grade)** — Age vs foveal crowding.
- **Repeated-letter crowding vs age (by grade)** — Age vs repeated-letter crowding.
- **Reading vs age (by grade)** — Age vs ordinary reading speed.
- **RSVP reading vs age (by grade)** — Age vs RSVP speed.
- **Foveal acuity vs age (by grade)** — Age vs foveal acuity.
- **Peripheral acuity vs age (by grade)** — Age vs peripheral acuity.
- **Foveal and peripheral acuity vs age** — Age vs acuity, foveal and peripheral series.
- **Foveal and peripheral crowding vs age** — Age vs crowding, foveal and peripheral series.

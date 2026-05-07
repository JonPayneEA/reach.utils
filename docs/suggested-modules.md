# Suggested future modules

This document describes four modules proposed for the reach ecosystem that
arose from a review of functionality useful to a flood forecast modelling team
and flood warning practitioners. Two are appropriate for **reach.utils** (general
utilities); two are better placed in **reach.validate** (forecast verification
and validation tooling).

---

## reach.utils candidates

### `thresholds.R`

Utility functions for detecting threshold crossings and extracting flood events
from a time series. These sit naturally in reach.utils as they are general-purpose
operations on numeric series rather than forecast-specific logic.

| Function | Signature | Description |
|---|---|---|
| `threshold_crossings` | `(x, values, threshold, direction = c("up", "down", "both"))` | Returns a `data.frame(timestamp, direction)` of the instants when `values` crosses `threshold`. An "up" crossing is the first timestamp at or above the threshold after a period below it; "down" is the reverse. |
| `time_above_threshold` | `(x, values, threshold, by)` | Returns the total duration for which `values` is at or above `threshold`, expressed as a numeric in seconds (or formatted via `format_duration()`). |
| `extract_events` | `(x, values, threshold, min_duration = 0, gap_tolerance = 0)` | Identifies contiguous runs where `values` ≥ `threshold`. `gap_tolerance` merges events separated by a gap of ≤ N steps (preventing brief dips from splitting a single flood event). Returns `data.frame(event_id, start, peak_time, peak_value, end, duration_secs)`. |
| `is_above_threshold` | `(values, threshold)` | Simple logical vector: `TRUE` where `values ≥ threshold`. Thin wrapper, useful in pipeline assertions and filter expressions. |

---

### `peaks.R`

Utility functions for extracting hydrograph peaks and characterising flood
events. Feeds both frequency analysis and model verification workflows.

| Function | Signature | Description |
|---|---|---|
| `extract_peaks` | `(x, values, min_separation = "24 hours")` | Finds local maxima in `values`, enforcing a minimum separation between consecutive peaks. Prevents double-counting during multi-peaked events. Returns `data.frame(timestamp, value)`. |
| `annual_maxima` | `(x, values)` | Extracts the single highest value per EA water year (via `water_year()`). Returns `data.frame(water_year, timestamp, value)`. |
| `rising_limb` | `(x, values, start, peak)` | Extracts the portion of the series between `start` and `peak` (the rising limb of a flood hydrograph). Returns a subset `data.frame(timestamp, value)`. |
| `falling_limb` | `(x, values, peak, end)` | Extracts the portion of the series between `peak` and `end` (the recession limb). Returns a subset `data.frame(timestamp, value)`. |
| `time_to_peak` | `(x, values, start, peak)` | Returns the duration in seconds from `start` to the time of peak value. |

---

## reach.validate candidates

### `warnings.R`

EA flood warning level classification and transition detection. Belongs in
reach.validate as it is concerned with validating and tracking the state of
flood warnings rather than general data manipulation.

The EA uses four operational warning levels:

| Level | Meaning |
|---|---|
| **Flood Alert** | Flooding is possible. Be prepared. |
| **Flood Warning** | Flooding is expected. Immediate action required. |
| **Severe Flood Warning** | Severe flooding. Danger to life. |
| **All Clear** | No further flooding expected. |

| Function | Signature | Description |
|---|---|---|
| `classify_warning_level` | `(value, thresholds)` | Given a scalar `value` (stage or flow) and a named list `thresholds` with elements `alert`, `warning`, `severe`, returns the current EA warning level as a character string. |
| `warning_transitions` | `(x, values, thresholds)` | Applies `classify_warning_level` across the series and returns a `data.frame(timestamp, from_level, to_level)` of every level change. |
| `format_warning_status` | `(status)` | Normalises a warning status string to the canonical EA form (handles common abbreviations and mixed case). |

---

### `verify.R`

Forecast verification metrics for threshold-based flood forecasts. These are
clearly verification/validation functions and belong in reach.validate.

---

#### Contingency table

A contingency table summarises the agreement between a binary forecast and a
binary observation at a defined threshold. At each time step, the observation
and forecast are each classified as **event** (≥ threshold) or **no event**
(< threshold), producing one of four outcomes:

|  | **Observed: event** | **Observed: no event** |
|---|---|---|
| **Forecast: event** | Hit (H) | False Alarm (FA) |
| **Forecast: no event** | Miss (M) | Correct Negative (CN) |

**Definitions**

- **Hit** — both forecast and observation exceed the threshold. The event was
  predicted and occurred.
- **False Alarm** — the forecast exceeded the threshold but the observation did
  not. An event was predicted that did not occur.
- **Miss** — the observation exceeded the threshold but the forecast did not.
  An event occurred that was not predicted.
- **Correct Negative** — neither forecast nor observation exceeded the
  threshold. No event was predicted and none occurred.

**How it is computed**

Given a paired observed series and a forecast series (both at the same
timestamps), each time step `i` is classified as:

```
obs_event[i]  <- observed[i]  >= threshold
fct_event[i]  <- forecast[i]  >= threshold

H  = sum( obs_event &  fct_event)
FA = sum(!obs_event &  fct_event)
M  = sum( obs_event & !fct_event)
CN = sum(!obs_event & !fct_event)
```

**Derived skill scores**

| Score | Formula | Interpretation |
|---|---|---|
| POD — Probability of Detection | H / (H + M) | Fraction of actual events that were forecast. 1 = perfect. |
| FAR — False Alarm Ratio | FA / (H + FA) | Fraction of forecast events that did not occur. 0 = perfect. |
| CSI — Critical Success Index | H / (H + M + FA) | Overall skill; ignores correct negatives. 1 = perfect. Also called the Threat Score. |
| Bias | (H + FA) / (H + M) | > 1 = over-forecasting; < 1 = under-forecasting; 1 = unbiased. |
| POFD — Prob. of False Detection | FA / (FA + CN) | Fraction of non-events incorrectly forecast as events. 0 = perfect. |

**Important caveat — correct negatives dominate flood data.** For rare events
like severe flooding, CN will be very large relative to H, FA, M. Scores that
include CN (e.g. accuracy = (H+CN)/N) are therefore misleading and should be
avoided. CSI is preferred precisely because it excludes CN.

**Proposed functions**

| Function | Signature | Description |
|---|---|---|
| `contingency_table` | `(observed, forecast, threshold)` | Returns a named integer vector (or 2×2 matrix) with elements H, FA, M, CN. |
| `prob_of_detection` | `(observed, forecast, threshold)` | Scalar POD derived from `contingency_table`. |
| `false_alarm_ratio` | `(observed, forecast, threshold)` | Scalar FAR. |
| `critical_success_index` | `(observed, forecast, threshold)` | Scalar CSI / Threat Score. |
| `forecast_bias` | `(observed, forecast, threshold)` | Scalar frequency bias. |
| `verify_threshold` | `(observed, forecast, threshold)` | Convenience wrapper returning all four scores in a named numeric vector. |

---

#### Verify lead time

Lead time is the amount of notice a forecast provides before an observed flood
threshold crossing — the key operational skill measure for a warning team.

**Conceptual definition**

> How many hours before the observed threshold crossing did the forecast first
> correctly predict that the threshold would be exceeded?

**How it is computed**

Flood forecasts are issued repeatedly (e.g. hourly) and each run has a set of
valid times extending into the future (the forecast horizon). This creates a
grid of (issue time, lead time) pairs.

```
Issue time:   T+0    T+1    T+2    T+3    ...
              |------|------|------|------|
              forecast values at each valid time
```

For a single forecast event the algorithm is:

1. **Find the observed crossing** — identify `t_cross`, the first timestamp at
   which the observed series meets or exceeds the threshold.

2. **Scan forecast runs in reverse chronological order** — working backwards
   from `t_cross`, find the earliest issue time `t_issue` at which any valid
   time within that run predicts exceedance at or before `t_cross`.

3. **Lead time** = `t_cross − t_issue`.

If the first correct forecast was issued at `t_cross − 6h`, the lead time is
6 hours.

**What counts as a "correct" prediction**

A forecast run is considered a correct prediction of the event if at least one
of its valid times that falls at or before `t_cross + tolerance` has a forecast
value ≥ threshold. A small `tolerance` window (e.g. ±1 step) can be allowed to
avoid penalising minor timing errors.

**Edge cases**

| Situation | Handling |
|---|---|
| Threshold never crossed in observations | No lead time computed; return `NA`. |
| Threshold crossed in forecast but not observations | False alarm; not counted as lead time. |
| No forecast run predicted the event | Lead time = 0 (or negative if the crossing was already past before any correct forecast was issued). |
| Forecast issued after crossing already occurred | Lead time is negative — the warning came too late. |

**Multiple events**

For a dataset covering several flood events, `verify_lead_time` returns one
lead time per event. Summary statistics (mean, median, minimum) across events
then characterise the overall forecast system performance.

**Proposed functions**

| Function | Signature | Description |
|---|---|---|
| `verify_lead_time` | `(x_obs, obs_values, forecasts, threshold, tolerance = 0)` | Returns a `data.frame(event_id, t_cross, t_first_correct_issue, lead_time_secs)`. `forecasts` is a list of `data.frame(issue_time, timestamp, value)`, one per forecast run. |
| `summarise_lead_times` | `(lead_time_df)` | Returns mean, median, min, max, and fraction with lead time > 0 across the event data frame returned by `verify_lead_time`. |

---

## Summary

| Module | Proposed package | Rationale |
|---|---|---|
| `thresholds.R` | reach.utils | General-purpose series operations; no forecast-specific logic |
| `peaks.R` | reach.utils | General hydrograph utilities; feeds both modelling and verification |
| `warnings.R` | reach.validate | Concerned with validating and classifying warning states |
| `verify.R` | reach.validate | Forecast verification; inherently a validation task |

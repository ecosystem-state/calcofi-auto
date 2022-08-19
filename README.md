<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Pull
data](https://github.com/ecosystem-state/calcofi-auto/workflows/R-pull-data/badge.svg)](https://github.com/ecosystem-state/calcofi-auto/actions)

[![Make
grid](https://github.com/ecosystem-state/calcofi-auto/workflows/R-gen-grid/badge.svg)](https://github.com/ecosystem-state/calcofi-auto/actions)

[![Estimate
densities](https://github.com/ecosystem-state/calcofi-auto/workflows/R-make-indices/badge.svg)](https://github.com/ecosystem-state/calcofi-auto/actions)

[![Run DFA
models](https://github.com/ecosystem-state/calcofi-auto/workflows/R-run-dfa/badge.svg)](https://github.com/ecosystem-state/calcofi-auto/actions)
<!-- badges: end -->

## Overview

This repository is a demonstration of automatic index generation using
data from CalCOFI and ERDDAP. The index is generated by applying Dynamic
Factor Analysis (DFA) to the top \~ 50 species, using spring samples
collected 1985 - present.

## Results

We find that a model with 3 trends has better predictive accuracy than a
model with 1-2 trends.

<div class="figure">

<img src="figures/trends.jpeg" alt="Estimated trends for the CalCOFI community" width="480" />
<p class="caption">
Estimated trends for the CalCOFI community
</p>

</div>

<div class="figure">

<img src="figures/loadings.jpeg" alt="Estimated loadings for the CalCOFI community" width="528" />
<p class="caption">
Estimated loadings for the CalCOFI community
</p>

</div>

<div class="figure">

<img src="figures/fitted.jpeg" alt="Predicted and observed fits to the CalCOFI data" width="720" />
<p class="caption">
Predicted and observed fits to the CalCOFI data
</p>

</div>

## bayesdfa

For more on the approach used, check out the [bayesdfa R
package](https://fate-ewi.github.io/bayesdfa/)

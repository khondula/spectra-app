Shiny app for simulating reflectance spectra

Models being developed in https://github.com/khondula/spectra

## Absorbance

* Water - from buiteveld94
* CDOM - ref 443 nm, exponential decline w/ slope factor
* Non-algal material - ref 443, exponential decline w/ slope factor
* **Phytoplankton - not implemented yet. partition into 4 gaussians**

## Backscattering

* Water - from buiteveld94, assume bb 50%
* Chlorophyll - power law ref 443 nm, slope factor and backs ratio
* Sediment - power law ref 443 nm, slope factor and backs ratio

## Other factors

* surface effects factor = 0.544
* $C(\mu_0) = -6.29 \mu_0 + 0.975$

Ranges and initial values for CDOM, NAP, SED, CHL parameters from
[Defoin-Platel and Chami 2007](https://doi.org/10.1029/2006JC003847).

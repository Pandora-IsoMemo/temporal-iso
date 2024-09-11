# OsteoBioR (temporal-iso)

R Package to estimate isotopic values in the past and their change over time. 

### Access to online version:
- MAIN version: https://isomemoapp.com/app/osteo-bio-r
- BETA version: https://isomemoapp.com/app/osteo-bio-r-beta

### Help and installation instructions:
- https://github.com/Pandora-IsoMemo/osteo-bior/wiki

### Release notes:
- see `NEWS.md`

### Folder for online models
- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/osteo-bior/tree/main/inst/app/predefinedModels)

## Stan development

* to re-generate `R/rstanmodel.R` and the C++ Source Code in `src` use
  `rstantools::rstan_config()`
* After that you can install and compile the package e.g. `devtools::load_all()`
* The precompiled stanmodel objects will appear in a named list called
  `stanmodels` and you can call them with something like
  `rstan::sampling(stanmodels$foo, ...)`
* You can put any stan model file in to `inst/stan` 
* Header files belong into `inst/include`

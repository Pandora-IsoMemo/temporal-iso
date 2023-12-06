# OsteoBioR

## Version 23.10.1

### Bug Fixes
- fix for model down- and upload: add missing export to package functions

## Version 23.10.0

### New Features
- _Import of models_:
  - option to import models from Pandora platform

## Version 23.09.1

### New Features
- tab _Model_, section _Credibility intervals over time_: new checkbox to extend the x-axis labels
to the _lower_ and _upper x limit_ (#26)

## Version 23.04.1

### Bug Fixes
- add missing logic for default values if "Use renewal rates uncertainty" is unchecked (#24)
- add an error message if the number of rows or columns do not match between renewal rates and their
uncertainties
- add tests to check new logic

## Version 23.03.2

### Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

## Version 23.03.1

### Bug fixes
- add remote package to enable the _Import Data_ module

## Version 23.02.2

### Updates
- add more tryCatch statements

## Version 23.02.1

### New Features
- the _Import Data_ module is now imported from the new package DataTools (#15, PR #16)
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_
- now, sidebars are fixed with auto scroll in all tabs (iso-app #4)

## Version 23.01.1

### New Features
- option to use renewal rates uncertainties

## Version 22.11.1

### Updates
- added column names to exported interval data
- shifted the UI to load/save a model from the right sidebar to the main panel above the tabs
- the content of the input _Individual varaible_ is now used to create default model names instead 
of "Current"
- new model name and note in remote Test data containing only model inputs and data
    
### Bug Fixes

- fixed export of interval data (#12)
- fixed reactive behavior after model upload in the tab _Credibility intervals over time_ (#12)
- fixed error when trying to export credibility interval plot 
- fix update of min/max time in the tabs Time point estimates and Estimates for user defined interval

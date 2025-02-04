# OsteoBioR 25.02.0

## New Features
- option to add custom points to a plot
- option to use custom legend labels for the plot (#69)
- new logos and links in header

# OsteoBioR 24.11.0

## Updates
- integration of modules from _ChangeR_ package for break-point detection (#72)
- option to set the "x" and "y" columns for mcp modelling

# OsteoBioR 24.10.0

## Updates
- _Credibility intervals over time_ updates (availbale with shinyTools 24.10.0) (#51):
  - option to format and set ranges of a second y-axis analog to other axes
  - option to use transformations for the x or y axis in order to handle extreme values
  - update example for mathematical annotation in titles
  
## Bug Fixes
- removing duplicated warnings when plotting models with no samples (#55)

# OsteoBioR 24.09.3

## Updates
- break point detection (#49, #61)
  - new outputs in tab '2. MCP Modeling': plot, waic
  - new methods for model comparison in tab '3. Comparison of Models': _waic_, _heuristic_
- _Credibility intervals over time_: new option to use notation for sub- and superscripts in titles 
  of a plots, axis or the legend (#59)
  
## Bug Fixes
- fix missing filter for legend content in _Credibility intervals over time_ (#63)

# OsteoBioR 24.09.1

## Fixes
- Clean up of rstan files and src folder. Add command to generate cpp files to Dockerfile.

# OsteoBioR 24.08.2

## New Features
- option to run a break point detection based on the mcp package (#49, #61)

## Updates
- _Credibility intervals over time_ (#59):
  - secondary axis: set the range if "Range detected from data" is unchecked and a second axis is selected
  - option to choose a custom legend position

## Bug Fixes
- _Credibility intervals over time_ in secondary axis: fix that removes the axis if the model was removed from the input and the plot was recreated (#59)

# OsteoBioR 24.08.1

## Bug Fixes
- catch error when trying to display plot and table of models that failed and have no samples (#55)

# OsteoBioR 24.08.0

## Bug Fixes
- prevent dropUp of inputs for `Element` and `Time` variables in the tab _Model_ and other 
  'pickerInputs' (#56)

# OsteoBioR 24.04.1

## New Features
- when generating multiple plots in Credible intervals over time have the option to select which 
ones to show/hide (#45)
- when having two y-axes (one on the left and another one on the right of the plot). Have the option
to switch their positions. (#43)
- option to add/edit plot(s) legend
- table output of data for the "Credible intervals over time" plot (#43)
- option to export values for displayed graph under "Credibility intervals over time" (#43)
- option to export data for single plots or batch export (#43)
  - for the latter a single file is generated with data for different individuals separated by a 
  fully empty row.
  - individual ID’s are given in a first column and for all x values
  - a column gives row number (1,2,3…) for each estimate

# OsteoBioR 24.04.0

## New Features
- option to change symbol, color, size of points in "Model: Credibility intervals over time" (#43)

# OsteoBioR 24.01.1

## New Features
- option to use rownames of data as individual variable. This allows to use non-numeric values for 
  individuals since rownames do not need to be numeric (#35)

# OsteoBioR 24.01.0

## Bug Fixes
- fix a bug that prevented the usage of multiple cores during model estimation. In essence only a
 single core was used.

# OsteoBioR 23.12.2

## New Features
- _Import of models from Pandora_: 
  - display of "About" information that is associated to a selected Pandora Repository

## Bug Fixes
- _Import of models from Pandora_: 
  - an error message occurred when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

# OsteoBioR 23.12.0

## New Features
- option to use a fixed seed when fitting the model (#37)

# OsteoBioR 23.10.1

## Bug Fixes
- fix for model down- and upload: add missing export to package functions

# OsteoBioR 23.10.0

## New Features
- _Import of models_:
  - option to import models from Pandora platform

# OsteoBioR 23.09.1

## New Features
- tab _Model_, section _Credibility intervals over time_: new checkbox to extend the x-axis labels
to the _lower_ and _upper x limit_ (#26)

# OsteoBioR 23.04.1

## Bug Fixes
- add missing logic for default values if "Use renewal rates uncertainty" is unchecked (#24)
- add an error message if the number of rows or columns do not match between renewal rates and their
uncertainties
- add tests to check new logic

# OsteoBioR 23.03.2

## Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

# OsteoBioR 23.03.1

## Bug fixes
- add remote package to enable the _Import Data_ module

# OsteoBioR 23.02.2

## Updates
- add more tryCatch statements

# OsteoBioR 23.02.1

## New Features
- the _Import Data_ module is now imported from the new package DataTools (#15, PR #16)
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_
- now, sidebars are fixed with auto scroll in all tabs (iso-app #4)

# OsteoBioR 23.01.1

## New Features
- option to use renewal rates uncertainties

# OsteoBioR 22.11.1

## Updates
- added column names to exported interval data
- shifted the UI to load/save a model from the right sidebar to the main panel above the tabs
- the content of the input _Individual varaible_ is now used to create default model names instead 
of "Current"
- new model name and note in remote Test data containing only model inputs and data
    
## Bug Fixes
- fixed export of interval data (#12)
- fixed reactive behavior after model upload in the tab _Credibility intervals over time_ (#12)
- fixed error when trying to export credibility interval plot 
- fix update of min/max time in the tabs Time point estimates and Estimates for user defined interval

# CCPP
An R Concentration Calculation and Plotting Package (CCPP) for Los Gatos Gas Analyzer (LGGA).

## Description
Built for an individual LGGA day/run with control in triplicates and four sample replicates.

## Getting Started

### Dependencies
* Packages: `readxl`, `tcltk`, `ggplot2`, `data.table`, `plotly`, `tidyr`, `dplyr`, `stringr`, `lubridate`, `readr`.
* Excel file (.xlsx) (from template) with injection times
* Logs from LGGA (.txt)

### Installing
``` r
# install.packages("devtools")
devtools::install_github("gnxmanu/CCPP")
```
<!-- or
``` r
# install.packages("githubinstall")
githubinstall("CCPP")
```
 -->
or
``` r
# install.packages("remotes")
remotes::install_github("gnxmanu/CCPP")
```
### Executing program
* User will be prompted for required .xsls and .txt files as well as for the directory to export products.
* The function runs on predetermined parameters, though they can be specified out of default (see below).
``` r
library(CCPP)
CCPPs()
```

* A folder tree will be created in the indicated directory.
```bash
.
├── Figures
│   └── Day
│        └── CH4
│             └── All
│             └── All_base_peaks
│             └── All_WithoutControls
│             └── ZoomInToPlateaus
│        └── CO2
│             └── All
│             └── All_base_peaks
│             └── All_WithoutControls
│             └── ZoomInToPlateaus
├── CalculatedConcentrations.csv
```
* All plots (10 per species) will be exported in their respective folders (dates are printed as parent folder names and species and variant to child folders).
* A .csv is created with the calculated concentrations (corrected for dilution and considering plateaus), as well as some summary data.

## Arguments

| Option                    | Description                                                                | Type      | Default  | Required? |
| ------------------------- | -------------------------------------------------------------------------- | --------- | -------- | --------- |
| `path_main`               | Export path. If NULL, it asks the user in GUI.                             | `string`  | `NULL`   | No        |
| `path_day1`               | Import path of 'LGGA log.txt' for day 1. If NULL, it asks the user in GUI. | `string`  | `NULL`   | No        |
| `path_day2`               | Import path of 'LGGA log.txt' for day 2. If NULL, it asks the user in GUI. | `string`  | `NULL`   | No        |
| `path_day3`               | Import path of 'LGGA log.txt' for day 3. If NULL, it asks the user in GUI. | `string`  | `NULL`   | No        |
| `interval`                | Length of interval to average on. If NULL, 10 seconds.                     | `integer` | `10`     | No        |
| `base_interval`           | Offset before injection time input by the user                             | `integer` | `20`     | No        |
| `peak_interval`           | Offset before injection time input by the user                             | `integer` | `20`     | No        |
| `V_sample`                | Injected sample volume [L] (V_s).                                          | `integer` | `100E-6` | No        |
| `V_loop`                  | Loop Volume [L] (V_L). Equivalent to 92 mL.                                | `integer` | `92E-3`  | No        |
| `offset_initial_final`    | Offset after injection at which the raw data will be considered. [seconds] | `integer` | `60`     | No        |

## Authors
**Manuel V.**
<!-- **Manuel Velázquez** -->

[@gnxmanu](https://github.com/gnxmanu)

## Version History
* v0.1 –––––– (Alpha Release. September 13, 2021
* v0.2.1 –––– Beta Release. October 23, 2021
* v0.2.2 –––– Beta Release. November 15, 2021
* v1.0 –––––– Public Release. 31 January 2022

## License
This project is licensed under the [MIT] License - see the LICENSE.md file for details.

## Future Updates
* Alpha Release
  - [x] Alpha version on github

<!-- * Beta Release -->
<!--   - [ ] Produce Readme.Rmd -->
<!--   - [ ] GUI prompt to impor LGGA logs (.txt) -->
<!--   - [ ] Enable in-line parameter input -->
<!--   - [ ] Mute/ warnings regarding plotting axes -->

* v.1.0
  - [ ] Remove: "* New names:"                                                                                                                     
  - [ ] Remove: "* `` -> ...1"
  - [ ] Remove: "Scale for 'y' is already present. Adding another scale for 'y', which will replace the existing scale."
  - [ ] Remove: "There were 50 or more warnings (use warnings() to see the first 50)"
  - [ ] Print parameters into resulting .csv and plots
  - [ ] Rename variables
  - [ ] Fix pacman or utilise if/require iteratively on a string
  - [ ] Confirm packages utilisation
  - [ ] Loops for each injection and for each day (calculations and plotting)
  - [ ] Input and loop that will allow any controls and samples replicates as well as day-runs
  - [ ] Introduce website
  - [ ] Present example study case
  - [ ] Add rectangle shading to denote flush, inyection and stabilisation stages

<!-- ## Acknowledgments

Inspiration, code snippets, etc.
* [smth](https://github.com/gnxmanu) -->

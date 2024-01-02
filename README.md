# Simulations to Control Social Desirability using Multiple Indicators Multiple Causes with Peabody Quadruplets 

This repository contains code files referenced in the following paper:

Bastos, R. V. S., & Valentini, F. (2024). Peabody with MIMIC as a theoretical sound control for social desirability: simulations for Likert and Forced-Choice. Manuscript Submitted for Publication.

## quadSimple and quadSim

Researchers can input their model and see if there’s enough power, low bias and high coverage to estimate the social desirability of items outside of the quadruplets using two shiny apps: quadSimple and quadSim.

[quadSimple](https://peabody-mimic.shinyapps.io/quadSimple/) is a Shiny web app developed by Rafael V. S. Bastos for power, bias, and coverage analysis for parameter estimation in structural equation modeling using MIMIC-Quadruplets model. quadSimple is more user-friendly and requires little information regarding the instrument. The quadSimple is recommended to be used prior to the construction of an instrument, in order to give light to the required number of quadruplets they need to build.

[quadSim](https://peabody-mimic.shinyapps.io/quadSim/) is a Shiny web app developed by Rafael V. S. Bastos for power, bias, and coverage analysis for parameter estimation in structural equation modeling using MIMIC-Quadruplets model. quadSim is more precise and requires more information about the instrument, and it's recommended for scales where researchers already have information on its parameters.


## Code

Source code of the apps are available at: quadSimple.R and quadSim.R. Users can run quadSimple and quadSim locally in R Studio by downloading the source code file, opening it in R Studio, and then pressing “Run App”.

# ISG Simulator

A Shiny app for simulating and comparing statistical designs commonly used in
safety pharmacology studies. The app helps users explore the operating
characteristics (power, type I error, bias, coverage) of several trial designs
under user-specified assumptions, and visualize how power changes with sample
size and effect size.

🌐 **Live app:** https://bing-liu.shinyapps.io/safety-pharm-design-simulater/

## Designs supported

| Tab | Design |
|-----|--------|
| p1 | Parallel |
| p2 | Parallel with repeated measures (Parallel-RM) |
| p3 | Crossover (XOV) |
| p4 | Sequential (SEQ) |
| p5 | Ascending dose (ASC) |
| p6 | Modified ascending dose (Modified-ASC) |

For each design, the app provides three sub-tabs:

1. **Design layout** — preview of one simulated trial (animals, periods, treatments, baseline, response).
2. **Operating characteristics** — runs `n_sim` simulations and reports power, type I error, bias, CI coverage from a `gt` table.
3. **Power analysis** — closed-form power and sample-size curves (using `pwr_ana_hc_*` helpers), with optional adjustment for baseline measurements.

## Repository layout

```
ISG_simulator/
├── shiny_standalone/                 # The full standalone Shiny app (deployable)
│   ├── app.R                         # Entry point
│   ├── app_helper.R                  # Path helpers (app_root, app_source, app_file)
│   ├── ui/                           # ui_p0.R … ui_p6.R + pics/
│   ├── server/                       # server_p0.R … server_p6.R
│   ├── functions/                    # pwr_ana_hc_*.R + plot/ helpers
│   ├── script/                       # import_power_tb_sim.R + functions/
│   ├── outputs/                      # Pre-computed simulation results (.rds)
│   ├── deploy_app.R                  # rsconnect deployment driver
│   ├── publish_app.sh                # Wrapper around deploy_app.R
│   └── README.md                     # App-specific deployment notes
├── PR.sh                             # Pull-request workflow helper
└── ISG_simulator.Rproj               # RStudio project file
```

The app is fully self-contained: every `source()` resolves inside
`shiny_standalone/`, so the folder can be copied or deployed on its own.

## Running locally

From R / RStudio:

```r
setwd("shiny_standalone")
shiny::runApp()
```

From a shell:

```bash
cd shiny_standalone
R -e 'shiny::runApp(launch.browser = FALSE)'
```

### Required R packages

`shiny`, `shinydashboard`, `mathjaxr`, `bslib`, `gt`, `dplyr`, `pwr`,
`ggplot2`, `plotly`, `tidyverse`, `rstatix`, `emmeans`, `ggpubr`, `lme4`,
`parallel`, `doParallel`, `rsconnect`.

## Deploying to shinyapps.io

A token from https://www.shinyapps.io/admin/#/tokens must be registered once
per machine via `rsconnect::setAccountInfo()`. After that:

```bash
cd shiny_standalone
./publish_app.sh
```

`publish_app.sh` clears the cached deployment metadata and runs
[deploy_app.R](shiny_standalone/deploy_app.R), which reads `RSCONNECT_ACCOUNT`,
`SHINYAPPS_TOKEN`, and `SHINYAPPS_SECRET` from the environment.

## Contributing

This repository follows the pull-request workflow documented in
[PR.sh](PR.sh):

1. Create / switch to `my-feature-branch` tracking `origin/main`.
2. Pull, commit changes, push the feature branch.
3. Open a PR on GitHub and merge it there.
4. Switch back to `main`, pull, delete the local feature branch.

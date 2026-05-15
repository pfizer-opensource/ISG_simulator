# Standalone Shiny App for shinyapps.io Deployment

This is a standalone version of the Power Analysis Shiny app configured for deployment to shinyapps.io.

## Structure

The app is organized as follows:

```
shiny_standalone/
├── app.R                          # Main app file (entry point)
├── app_helper.R                   # Helper functions for path setup
├── ui/                            # User interface files
│   ├── ui_p0.R through ui_p5.R   # UI definitions for each section
│   └── pics/                      # Images used in the app
├── server/                        # Server logic files
│   ├── server_p0.R through server_p5.R  # Server logic for each section
├── functions/                     # Power analysis functions
│   ├── pwr_ana_hc_*.R            # Power analysis core functions
│   └── plot/                      # Plotting functions
└── script/                        # Helper scripts
    ├── import_power_tb_sim.R     # Data import functions
    └── functions/                # Additional utility functions
        ├── f_list_to_tb.R        # Table conversion
        └── scen_in/              # Scenario input functions
```

## How to Run Locally

1. **From R console:**
   ```R
   setwd("/path/to/shiny_standalone")
   shiny::runApp()
   ```

2. **From command line:**
   ```bash
   cd /path/to/shiny_standalone
   R -e "shiny::runApp()"
   ```

3. **From RStudio:**
   - Open `app.R`
   - Click "Run App" button

## Deploying to shinyapps.io

### Prerequisites

1. Install required packages:
   ```R
   install.packages("rsconnect")
   install.packages("shiny")
   install.packages("shinydashboard")
   # ... install all other dependencies listed in app.R
   ```

2. Set up shinyapps.io account:
   - Create account at https://www.shinyapps.io
   - Generate authorization token

### Deploy

1. **Authorize with shinyapps.io:**
   ```R
   rsconnect::setAccountInfo(name="your-account-name", 
                             token="your-token", 
                             secret="your-secret")
   ```

2. **Deploy the app:**
   ```R
   setwd("/path/to/shiny_standalone")
   rsconnect::deployApp(appName = "power-analysis-app")
   ```

   Or use the shorthand:
   ```R
   rsconnect::deployApp()
   ```

3. **Verify deployment:**
   - Visit `https://your-account-name.shinyapps.io/power-analysis-app`
   - Test all functionality

### Updating after deployment

1. Make changes to files in `shiny_standalone/`
2. Redeploy using the same command:
   ```R
   rsconnect::deployApp()
   ```

## File Organization Notes

All paths in this app have been converted to work with the standalone folder structure:

- Original path: `"shiny/script/functions/scen_in/0918/..."`
  - Standalone: `"script/functions/scen_in/0918/..."`

- Original path: `"function/plot/..."`
  - Standalone: `"functions/plot/..."`

The `app_helper.R` file provides helper functions (`source_root` and `project_root`) that work with the new structure. The main `app.R` file uses direct relative paths for sourcing files.

## Troubleshooting

### Error: Cannot find file

- Ensure all R files are properly copied to their corresponding directories
- Check that image files exist in `ui/pics/`
- Verify paths in source() calls use the new relative path structure

### Missing dependencies

- Install all packages listed at the top of `app.R`
- For parallel processing: `doParallel`, `parallel`

### Resource path issues

- The app uses `addResourcePath("pics", ...)` to serve images
- Ensure all PNG/image files are in the `ui/pics/` directory
- Reference images in UI as `src="pics/filename.png"`

## Original vs Standalone Differences

The standalone version simplifies the original app structure:

1. **Path Setup**: Original used complex logic to find project root; standalone uses simple relative paths from app directory

2. **Dependencies**: Original sourced files from across the full project; standalone contains only necessary files

3. **Working Directory**: Original changed working directory during app startup; standalone maintains app root directory

4. **Compatibility**: Standalone version is optimized for shinyapps.io but can also run locally

## Support

For issues specific to deployment, consult:
- shinyapps.io documentation: https://docs.posit.co/shinyapps.io/
- Shiny documentation: https://shiny.rstudio.com/

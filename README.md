# Atlas of Living Australia | Science & Decision Support
This is a working repository for R code produced by the ALA Science & Decision Support Team. The team consists of:

- Martin Westgate (Team Lead)
- Dax Kellie
- Shandiya Balasubramaniam
- Amanda Buyan
- Juliet Seers

## Folder structure
The repo stores two main types of outputs:
- */comms* contains code for one-off diagrams built for outreach purposes
  - Activities are stored in folders with 'year-month_description' naming format
- */projects* contains larger pieces of analysis
  - Each project has its' own folder
  - A single project consists of scripts in .R format, and may contain standard subfolders for R projects ('functions', 'plots', 'data', etc.)

## Code styling
Our default packages are:
- {galah} | Interface with ALA data
- {sf} | Spatial analysis
- {terra} | Raster analysis
- {devtools} | Package development
- {ggplot2} + extensions | Plotting
- {shiny} + {bslib} | Apps

## Re-use
You can use anything you find here in your own work (at your own risk!). Some points to be aware of:

- Some scripts may be obsolete
  - i.e. Updating of old code is rare
- Large or sensitive data are not archived
  - i.e. Not every script is perfectly self-contained
- Our emphasis is on robust, standardised methods
  - i.e. We don't provide much guidance for cutting-edge or exploratory statistical methods

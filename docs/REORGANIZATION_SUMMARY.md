# ESCAPE Project Reorganization Summary

## Date
2026-02-27

## Overview
This document summarizes the comprehensive audit and reorganization of the ESCAPE project repository to improve maintainability, navigation, and adherence to industry best practices.

## Changes Made

### 1. Archive Cleanup

#### Files Moved to Archive
The following obsolete, deprecated, and unused files were moved to the `archive/` directory:

**System/Temporary Files (archived in `archive/temp/` - later removed):**
- `.DS_Store` - macOS system file
- `.RData` - R workspace data
- `.Rhistory` - R command history
- `sampleData.csv` - Duplicate of `data/sampleData.csv`

**Documentation (archived in `archive/docs/`):**
- `Zhang2018_Frontiers.pdf` - Reference paper from original publication

**Obsolete Assets (archived in `archive/obsolete/`):**
- `assets/logo.png` - Duplicate of `www/assets/logo.png`
- `assets/` - Duplicate assets directory

**Deployment Artifacts (archived in `archive/deployment/`):**
- `expectancyApp/` - rsconnect deployment files (ShinyApps.io artifacts)

**Deprecated App Versions (archived in `archive/versions/`):**
- `appv2.r` - Original single-file app (2018)
- `app_v2.R` - Second iteration
- `app_v3.R` - Third iteration

### 2. Documentation Reorganization

The following documentation files were moved to the `docs/` directory for better organization:
- `README.md` - Project documentation
- `plan.md` - Enhancement plan and feature roadmap
- `shinyInstructions.html` - In-app help documentation

### 3. Code Updates

**app.R Updates:**
- Updated the reference to `shinyInstructions.html` from root to `docs/shinyInstructions.html` (line 1452)

### 4. New Files Created

**.gitignore:**
Created a comprehensive `.gitignore` file to prevent system files, temporary files, and IDE-specific files from being committed to version control.

## Final Project Structure

```
ShinyAESC/
├── .gitignore                 # Git ignore rules
├── app.R                     # Main Shiny application
├── .claude/                  # Claude AI assistant files
├── archive/                   # Archived files
│   ├── README.md             # Archive documentation
│   ├── versions/             # Deprecated app versions
│   │   ├── appv2.r
│   │   ├── app_v2.R
│   │   └── app_v3.R
│   ├── obsolete/             # Obsolete files
│   │   ├── logo.png
│   │   └── assets/
│   ├── deployment/           # Deployment artifacts
│   │   └── expectancyApp/
│   └── docs/                # Archived documentation
│       └── Zhang2018_Frontiers.pdf
├── data/                     # Data files
│   └── sampleData.csv       # Sample dataset
├── docs/                     # Documentation
│   ├── README.md            # Project documentation
│   ├── plan.md              # Enhancement plan
│   ├── shinyInstructions.html # In-app help
│   └── REORGANIZATION_SUMMARY.md # This file
├── R/                        # R utility scripts
│   ├── utils_theme.R        # Theme and UI styling
│   ├── utils_stats.R        # Statistical calculations
│   └── utils_plots.R        # Plotting functions
└── www/                      # Web assets
    ├── assets/              # Static assets
    │   └── logo.png
    ├── css/                 # Stylesheets
    │   ├── landing.css
    │   └── main.css
    └── js/                  # JavaScript
        └── main.js
```

## Benefits of Reorganization

1. **Improved Navigation**: Files are now organized by purpose (application code, utilities, data, documentation, web assets)
2. **Better Maintainability**: Clear separation of concerns makes it easier to find and update files
3. **Reduced Clutter**: Obsolete and duplicate files are archived, reducing confusion
4. **Version Control**: `.gitignore` prevents temporary and system files from being committed
5. **Standard Structure**: Follows R Shiny project best practices and industry conventions

## Active Files Reference

### Core Application
- `app.R` - Main application entry point

### R Utilities
- `R/utils_theme.R` - Theme configuration and UI styling
- `R/utils_stats.R` - Statistical calculations (correlation, Cohen's d, CLES, BESD)
- `R/utils_plots.R` - Plotting functions (scatter, histogram, expectancy, icon array)

### Data
- `data/sampleData.csv` - Sample dataset for demonstration

### Web Assets
- `www/css/main.css` - Main application styles
- `www/css/landing.css` - Landing page styles
- `www/js/main.js` - Client-side JavaScript
- `www/assets/logo.png` - Application logo

### Documentation
- `docs/README.md` - Project overview and usage instructions
- `docs/plan.md` - Enhancement plan and feature roadmap
- `docs/shinyInstructions.html` - In-app help documentation

## Verification

All file references in `app.R` have been verified and updated:
- ✓ `R/utils_theme.R` (line 15)
- ✓ `R/utils_stats.R` (line 16)
- ✓ `R/utils_plots.R` (line 17)
- ✓ `data/sampleData.csv` (lines 26-29)
- ✓ `www/css/main.css` (line 786)
- ✓ `www/css/landing.css` (line 787)
- ✓ `www/js/main.js` (line 793)
- ✓ `docs/shinyInstructions.html` (line 1452)

## Next Steps

1. Test the application to ensure all functionality works correctly
2. Update any deployment scripts to reflect the new structure
3. Consider adding a `tests/` directory for unit tests
4. Consider adding a `scripts/` directory for utility scripts

## Notes

- The archive directory is kept for historical reference and can be removed if not needed
- The `.gitignore` file should be reviewed and adjusted based on team preferences
- All active files follow R Shiny project conventions and industry best practices

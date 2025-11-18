# AlloMate - Genetic Breeding Optimization App

## 🧬 Overview

AlloMate is a Shiny web application for genetic breeding optimization using Optimum Contribution Selection (OCS). The app helps breeders make informed decisions about mate selection and breeding strategies by analyzing kinship relationships and breeding values.

## 🚀 Features

### Core Functionality
- **Kinship Analysis**: Calculate and visualize kinship relationships between potential mates
- **EBV Processing**: Combine multiple breeding value traits with user-defined weights
- **Optimum Contribution Selection**: Optimize breeding contributions while controlling inbreeding
- **Mating Plan Generation**: Create optimal mating pairs to minimize inbreeding with multiple strategies
- **Excel Export**: Download comprehensive results in Excel format with multiple worksheets

### Advanced OCS Options
- **Per-Pair Kinship Enforcement**: Filter mating pairs based on individual kinship thresholds
- **Greedy Mating Algorithm**: Browser-safe optimization for web deployment (WebR compatible)
- **Pure R Heuristic**: Experimental approach bypassing quadratic programming for testing
- **Multiple Solver Backends**: Automatic fallback between optiSel, quadprog, and custom implementations

### User Interface Features
- **Interactive Help System**: Comprehensive in-app documentation with table of contents
- **Dynamic Startup Guide**: Step-by-step progress tracking with visual feedback
- **File Status Monitoring**: Real-time tracking of uploaded files and processing status
- **Pedigree Validation**: Detailed statistics on data quality including duplicates, circular references, and missing parents
- **R Code Export**: Download complete R script for standalone analysis
- **Smart Error Handling**: Contextual error messages with troubleshooting hints

### Technical Features
- **Dual Implementation**: Works with both optiSel package and custom fallback
- **WebR Compatible**: Runs in web browsers without server installation
- **Modular Design**: Clean, organized codebase for easy maintenance
- **Robust Error Handling**: Comprehensive validation and user feedback
- **Pure R Excel Writer**: Fallback Excel export when openxlsx is unavailable
- **Package Flexibility**: Graceful degradation when optional packages are missing

## 📁 Project Structure

```
AlloMate/
├── app/                      # Shiny application
│   ├── global.R              # Package loading and environment detection
│   ├── ui.R                  # User interface with dynamic components
│   ├── server.R              # Server logic and reactivity
│   ├── R/                    # Modular function library
│   │   ├── load_functions.R  # Centralized function loader
│   │   ├── utils.R           # Data processing utilities
│   │   ├── ocs_helpers.R     # OCS analysis functions
│   │   ├── ui_helpers.R      # UI generation helpers
│   │   ├── optsel_fallback.R # Custom OCS implementation
│   │   └── pure_xlsx_writer.R # Pure R Excel export
│   ├── www/                  # Static web resources
│   │   ├── allomate.png      # App logo
│   │   ├── logos.png         # Partner logos
│   │   └── logos2.png        # Updated partner logos
│   ├── rsconnect/            # Deployment configuration
│   └── README.md             # This documentation file
├── data/                     # Sample and test datasets
│   ├── original_USDA_data/   # USDA ARS trout breeding data
│   ├── play_data/            # Testing dataset with fake IDs
│   ├── riverence/            # Commercial breeding data
│   └── usda-ars_trout/       # Additional USDA datasets
├── scripts/                  # Development and validation tools
│   ├── run_app.R             # Local app launcher
│   ├── validate_ocs_logic.R  # OCS algorithm validation
│   ├── compare_ocs_results.R # Compare optiSel vs custom
│   ├── generate_fake_pedigree.R # Create test data
│   ├── generate_fake_ebvs.R  # Create test EBV files
│   └── export_shinylive.R    # WebR deployment export
├── docs/                     # WebR/Shinylive deployment files
│   ├── AlloMate/             # App bundle for web
│   └── shinylive/            # WebR runtime files
├── manuscript/               # Publications and documentation
└── README.md                 # Project overview (links to app README)
```

### Key Files Explained

**Core Application Files:**
- `global.R`: Handles package installation, environment detection (WebR vs local), and function loading
- `ui.R`: Defines the user interface with collapsible panels, help system, and dynamic inputs
- `server.R`: Contains all reactive logic, data processing, and export handlers

**Function Modules (app/R/):**
- `load_functions.R`: Ensures all function files are loaded in correct order
- `utils.R`: Data reading, pedigree cleaning, kinship calculation, EBV processing
- `ocs_helpers.R`: OCS analysis workflow, result formatting, Excel export
- `ui_helpers.R`: Dynamic UI generation, status displays, validation helpers
- `optsel_fallback.R`: Complete custom OCS implementation when optiSel unavailable
- `pure_xlsx_writer.R`: Pure R Excel writer for environments without openxlsx

**Data Organization:**
- Each dataset folder contains: candidates, pedigree, and EBV files
- Multiple trait files supported (weight, length, width, lactate, etc.)
- Fake data versions available for testing without real identifiers

## 🛠️ Installation

### Prerequisites
- R (version 4.0 or higher)
- Required R packages (automatically installed):
  - shiny, readr, dplyr, tidyr, purrr, kinship2, DT, tibble, openxlsx, quadprog

### Local Installation
1. Clone or download the repository
2. Open R or RStudio
3. Set working directory to the AlloMate folder
4. Run the app:
   ```r
   shiny::runApp("app")
   ```

### Package Installation
The app automatically handles package installation when it starts up:

#### How it Works
1. **global.R** runs once when the Shiny app starts
2. It checks for all required packages and installs missing ones
3. For `optiSel` specifically, it tries CRAN first, then Bioconductor if needed
4. Installation status is displayed in the app interface

#### Troubleshooting Package Installation

**If optiSel Installation Fails:**

**Option 1: Manual Installation (Recommended)**
```r
# Try CRAN first
install.packages("optiSel")

# If that fails, try Bioconductor
if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("optiSel")
```

**Option 2: Check System Requirements**
- Ensure you have write permissions to your R library directory
- Check that you have sufficient disk space
- Verify your internet connection

**Option 3: Update R and Packages**
```r
# Update R to latest version
# Then update all packages
update.packages(ask = FALSE)
```

### WebR Deployment
The app is compatible with WebR environments and will automatically use the custom OCS fallback when optiSel is not available.

## 📊 Usage

### 1. Upload Data
- **Candidates File**: Upload a text file with columns `id` and `sex` (M/F)
- **Pedigree File**: Upload a text file with columns `id`, `sire`, and `dam`
  - App automatically validates pedigree quality
  - Displays statistics on unknown parents, circular references, and duplicates
- **EBV Files**: Upload breeding value files with columns `ID` and `EBV`
  - Add multiple traits using the "➕ Add trait" button
  - Assign weights to each trait (must sum to 1.0)

### 2. Configure Analysis
- **Traits**: Add multiple EBV traits with relative weights (must sum to 1)
- **Kinship Threshold**: Set maximum allowed kinship between mates
- **OCS Parameters**: Set desired inbreeding rate and number of offspring

### 3. Configure OCS Options (Advanced)
The app offers three checkbox options for customizing the OCS analysis:

#### Enforce Per-Pair Kinship Threshold
- **Default**: Enabled (checked)
- **Purpose**: Forces the mating solver to drop any pairing whose kinship exceeds the specified limit
- **When to use**: Keep enabled for stricter inbreeding control at the mate-pair level
- **Technical details**: 
  - The OCS phase still optimizes contributions subject to the global kinship constraint
  - The mating solver now respects the threshold directly; infeasible settings raise a clear error
  - Ensures no single mating exceeds your kinship limit and keeps offspring counts aligned with the plan
  - If no feasible plan exists under the current limit, increase the threshold or lower the offspring target

#### Use Greedy Mating (Browser-Safe)
- **Default**: Disabled locally, automatically enabled in WebR environments
- **Purpose**: Uses a greedy algorithm instead of linear programming for mating plan generation
- **When to use**: 
  - Always enable for WebR/browser deployments
  - Enable if you encounter solver issues with large datasets
  - Keep disabled for optimal results in standard R environments
- **Technical details**: 
  - The greedy algorithm sorts candidates by contribution and assigns mates iteratively
  - Produces near-optimal results without requiring lpSolve package
  - Faster for very large populations (>500 candidates)
  - May not achieve perfect optimality but provides robust, reliable results

#### Bypass Quadprog (Heuristic Contributions)
- **Default**: Disabled
- **Purpose**: Uses a simple heuristic instead of quadratic programming for calculating optimal contributions
- **When to use**: 
  - ⚠️ **Testing and debugging only** - not recommended for production analyses
  - Use if quadprog fails due to numerical issues
  - Useful for understanding algorithm behavior
- **Technical details**: 
  - Assigns equal contributions to all candidates (ignores breeding values)
  - **WARNING**: Does not respect inbreeding constraints
  - Provides a baseline for comparison but not optimized results
  - Only recommended when troubleshooting solver issues

### 4. View Results
- **Kinship Matrix**: Visualize kinship relationships with color coding
- **EBV Matrix**: View combined breeding values for potential crosses
  - Filtered to show only viable crosses (positive EBV, kinship below threshold)
- **OCS Results**: See optimal candidate contributions and mating plan
  - Candidate table shows optimal contribution percentages and offspring counts
  - Mating table shows recommended pairs with kinship values
  - Solver notes indicate which algorithm was used

### 5. Export Results
- Download comprehensive Excel files with all results
- Includes multiple worksheets:
  - README with analysis details
  - Filtered Results (crosses meeting all criteria)
  - EBV Matrix (complete matrix view)
  - OCS Candidates (when OCS is run)
  - Mating Plan (when OCS is run)
  - Parameters used in analysis

### 6. Access Documentation
- Click "❓ Help" button to view complete documentation in-app
- Click "📝 View R Code" to see and download the complete implementation
- Use the table of contents for quick navigation to specific topics

## 🔧 Technical Details

### OCS Implementation
The app uses a multi-tiered implementation approach with automatic fallbacks:

#### Primary: optiSel Package (Preferred)
1. App attempts to install and load optiSel from CRAN
2. If successful, uses native optiSel functions for optimal performance
3. Provides access to full suite of optimization features
4. Status shows: "✅ optiSel package is available"

#### Secondary: Custom OCS Fallback
1. Automatically activated if optiSel is unavailable (e.g., in WebR)
2. Loads custom implementations from `R/optsel_fallback.R`
3. Provides compatible interface: `custom_candes`, `custom_opticont`, `custom_noffspring`, `custom_matings`
4. Uses quadprog for contribution optimization (if available)
5. Uses lpSolve for mating plan (if available), otherwise uses greedy algorithm
6. Status shows: "✅ Custom OCS fallback is available"

#### Tertiary: Pure R Heuristics
1. Used when neither optiSel nor optimization packages are available
2. Provides basic functionality without external dependencies
3. Suitable for testing and understanding algorithm behavior
4. **Not recommended for production analyses**

#### Algorithm Selection Priority
The app automatically selects the best available approach:
1. **Contribution Optimization**: optiSel > quadprog > equal weights heuristic
2. **Mating Plan**: optiSel > lpSolve > greedy algorithm
3. In WebR environments, greedy mating is automatically enabled regardless of lpSolve availability

### Data Processing Pipeline
1. **Candidate Loading**: Reads and validates candidate files
   - Standardizes ID formats
   - Validates sex coding (M/F)
   - Checks for required columns

2. **Pedigree Cleaning**: 
   - Removes duplicate individuals (keeps first occurrence)
   - Breaks circular dependencies
   - Fixes messy parent assignments (individuals appearing as both sire and dam)
   - Standardizes unknown parent coding
   - Reports detailed validation statistics

3. **Kinship Calculation**:
   - Uses kinship2 package when available
   - Falls back to identity matrix approximation if needed
   - Subset to selected candidates for efficiency

4. **EBV Processing**:
   - Combines multiple trait files with user-defined weights
   - Handles missing values (replaced with 0)
   - Validates weight totals (must sum to 1.0)
   - Filters EBVs to match candidates

5. **OCS Analysis**:
   - Validates inputs (sex balance, data completeness)
   - Constructs optimization problem
   - Solves for optimal contributions
   - Generates mating plan
   - Applies optional per-pair kinship filtering

### Performance Characteristics
- **Small datasets** (<100 candidates): 
  - Fast with all implementations
  - optiSel and custom fallback provide similar results
  - Complete analysis in <5 seconds

- **Medium datasets** (100-500 candidates): 
  - optiSel: Excellent performance (<10 seconds)
  - Custom fallback: Good performance (<30 seconds)
  - Greedy mating recommended for WebR

- **Large datasets** (>500 candidates): 
  - optiSel: Good performance (<60 seconds)
  - Custom fallback: May be slower (1-3 minutes)
  - Greedy mating strongly recommended for WebR
  - Consider kinship matrix subsetting for very large pedigrees

### Browser Compatibility
- Tested in Chrome, Firefox, Safari, and Edge
- WebR deployment requires greedy mating option
- Excel downloads use workaround for Chrome's download attribute bug
- Responsive design for various screen sizes



## 📈 Sample Results

With the provided sample data:
- **84 candidates** (63 males, 21 females)
- **66 candidates selected** for breeding
- **97 optimal mating pairs** generated
- **Mean offspring inbreeding**: 0.0053
- **Expected genetic gain**: +0.0557 (improved from -0.0290 baseline)

## 🔍 Troubleshooting

### Common Issues and Solutions

#### Data Upload Issues

1. **"Missing required columns" error**
   - **Candidates file**: Must have columns named `id` and `sex`
   - **Pedigree file**: Must have columns named `id`, `sire`, and `dam`
   - **EBV files**: Must have columns named `ID` and `EBV`
   - Check for extra spaces or capitalization differences in column names

2. **"No valid EBV files" error**
   - Ensure all trait files are uploaded before clicking "Run OCS"
   - Verify each EBV file has both ID and EBV columns
   - Check that weights are numeric and sum to 1.0

3. **"Candidates lack corresponding EBVs" warning**
   - Some candidate IDs don't have matching EBV records
   - These candidates will be excluded from analysis
   - Download the missing IDs list using the provided link
   - Either add missing EBVs or remove candidates from candidate file

#### Pedigree Issues

4. **"Circular reference detected" warning**
   - Individual appears as its own parent
   - App automatically breaks these at earliest generation
   - Review pedigree data for data entry errors

5. **"Duplicate IDs removed" warning**
   - Multiple pedigree records with same ID
   - App keeps first occurrence only
   - Check source data for duplicate entries

6. **"Unknown parent(s)" warning**
   - Some individuals have missing sire or dam
   - Treated as founders (no parents)
   - Normal for base population animals

#### OCS Analysis Issues

7. **"Only one sex selected" error**
   - OCS optimization resulted in all males or all females being selected
   - **Solutions**:
     - Increase the inbreeding rate threshold (try 0.10 or higher)
     - Reduce the number of requested offspring
     - Check that both sexes have positive breeding values
     - Verify kinship matrix is reasonable

8. **"No feasible OCS solution found" error**
   - Candidate population too related to meet inbreeding constraint
   - **Solutions**:
     - Increase desired inbreeding rate (e.g., from 0.05 to 0.10)
     - Reduce number of offspring
     - Add less related candidates if possible
     - Review kinship matrix for errors

9. **"Optimization failed: Constraints not met" error**
   - Numerical issues in optimization solver
   - **Solutions**:
     - Try enabling "Use greedy mating" checkbox
     - Increase inbreeding rate threshold
     - Check for extreme values in kinship matrix
     - Verify all candidates have finite breeding values

#### Package and Environment Issues

10. **"Package not available" error**
    - The app will automatically use custom fallback
    - Check R console messages for details
    - Some features may work differently but should still function

11. **"Requested package not found in webR binary repo"**
    - Normal in WebR environments
    - App automatically uses browser-safe fallbacks
    - Enable "Use greedy mating" checkbox for best results

12. **"Permission denied" during package installation**
    - You don't have write access to R library directory
    - **Solutions**:
      - Run R/RStudio as administrator (Windows)
      - Use `sudo R` in terminal (Linux/Mac)
      - Change library location: `.libPaths(new_path)`

#### Export Issues

13. **Excel download fails or is empty**
    - Check that at least candidates and pedigree are uploaded
    - Verify EBV matrix was successfully generated
    - Look for error messages in startup guide
    - Try refreshing the page and re-uploading data

14. **Excel file opens with errors**
    - May occur if using pure R export fallback
    - File should still be readable, Excel shows warning
    - Try opening with LibreOffice if issues persist

#### Performance Issues

15. **App is slow or unresponsive**
    - Large datasets take longer to process
    - For >500 candidates, enable "Use greedy mating"
    - Consider reducing pedigree size if possible
    - Check browser console for JavaScript errors

16. **OCS analysis takes very long**
    - Expected for large datasets with custom fallback
    - optiSel package is much faster for large analyses
    - Enable greedy mating for WebR deployments
    - Loading spinner indicates progress

### Debug Mode and Diagnostics

**Console Messages**: The app logs detailed progress messages to the R console:
- Package loading status
- Data processing steps
- OCS solver selection and progress
- Warning and error details

**Startup Guide**: The dynamic startup guide shows:
- ✅ Completed steps
- ⬜ Pending steps  
- ❌ Steps with errors

**File Status Box**: Shows real-time status of:
- Candidate list processing
- Pedigree validation
- EBV matrix generation
- OCS results availability

**Help Tab**: Access complete documentation anytime by:
- Clicking "❓ Help" button in sidebar
- Navigating to the "Help" tab
- Using table of contents to jump to specific topics

### Getting Additional Help

If problems persist:
1. Check the R console output for detailed error messages
2. Review the "Help" tab for complete documentation
3. Verify input file formats match requirements exactly
4. Try with provided sample data to confirm app works
5. Look for solver notes in OCS results indicating algorithm used

## 🏗️ Code Organization

### Function Structure
The app uses an organized functions structure to improve code maintainability, reusability, and clarity:

#### Function Categories
1. **Data Processing Functions** (`app/R/utils.R`)
   - Handle all data input, cleaning, and processing operations
   - Functions: `read_candidates()`, `clean_pedigree()`, `compute_kinship_matrix()`, `process_ebvs()`, `calculate_index()`

2. **OCS Functions** (`app/R/ocs_helpers.R`)
   - Handle all Optimum Contribution Selection operations
   - Functions: `run_ocs()`, `validate_ocs_inputs()`, `format_ocs_results()`, `create_ocs_workbook()`

3. **UI Helper Functions** (`app/R/ui_helpers.R`)
   - Handle UI elements, reactive values, and display formatting
   - Functions: `create_trait_inputs()`, `generate_package_status()`, `format_kinship_ebv_results()`, `validate_file_upload()`

4. **Function Loader** (`app/R/load_functions.R`)
   - Centralized loading of all function files in the correct order

### Benefits of Organization
- **Maintainability**: Functions grouped by purpose, easy to locate and modify
- **Reusability**: Functions can be used across different parts of the app
- **Testing**: Individual function files can be tested separately
- **Documentation**: Each function file has clear documentation
- **Collaboration**: Multiple developers can work on different function files

## 🤝 Contributing

### Code Organization
- Follow the established project structure
- Add new functions to appropriate R/ files
- Update documentation for new features
- Test thoroughly before submitting

### Development Guidelines
- Use Roxygen2 comments for all functions
- Follow R naming conventions
- Include error handling and validation
- Test with both optiSel and fallback implementations

### Function Documentation
All functions include Roxygen2 documentation with:
- Parameter descriptions
- Return value details
- Usage examples
- Error handling information

## 📄 License

This project is developed for genetic breeding optimization research and applications.

## 🙏 Acknowledgments

- Built with R and Shiny
- Uses kinship2 for pedigree analysis
- Custom OCS implementation for WebR compatibility
- Sample data provided for testing and demonstration

## 🆕 Recent Updates and New Features

### Version 2.0 Features (Current)

**Enhanced User Experience:**
- 🎨 **Dynamic Startup Guide**: Real-time progress tracking with visual status indicators (✅/⬜/❌)
- 📊 **File Status Monitoring**: Live dashboard showing upload and processing status for all data files
- 📚 **Interactive Help System**: Complete documentation accessible in-app with searchable table of contents
- 📝 **R Code Export**: Download complete, standalone R scripts for reproducible analysis
- 🔄 **Smart Error Handling**: Context-aware error messages with specific troubleshooting suggestions

**Data Quality and Validation:**
- 🧹 **Enhanced Pedigree Cleaning**: Automatic detection and handling of circular references, duplicates, and messy parents
- 📈 **Validation Statistics**: Detailed reporting on pedigree quality metrics
- 🔍 **Missing Data Tracking**: Downloadable lists of candidates without EBVs
- ⚠️ **Warning System**: Color-coded alerts for data quality issues

**OCS Algorithm Improvements:**
- 🎯 **Per-Pair Kinship Enforcement**: Optional filtering to ensure no mating exceeds kinship threshold
- 🌐 **Greedy Mating Algorithm**: Browser-safe mating plan generation for WebR deployment
- 🔬 **Pure R Heuristic**: Testing mode bypassing quadratic programming (experimental)
- 🔄 **Multi-Tier Fallback**: Automatic selection of best available solver (optiSel > quadprog > heuristic)
- 📊 **Solver Transparency**: Results include notes on which algorithm was used

**Export and Reporting:**
- 📑 **Multi-Sheet Excel Export**: Comprehensive results with README, parameters, filtered results, and full matrices
- 💾 **Pre-Built Export Cache**: Faster downloads with pre-generated Excel files
- 🔧 **Pure R Excel Writer**: Functional export even without openxlsx package
- 📋 **Detailed README Sheets**: Every export includes explanation of contents and parameters used

**WebR and Browser Deployment:**
- 🌐 **Full WebR Compatibility**: Runs entirely in browser without R installation
- ⚡ **Automatic Environment Detection**: Adjusts behavior based on deployment type
- 🔄 **Graceful Package Degradation**: Works with varying package availability
- 🎯 **Optimized for Chrome**: Workarounds for browser-specific download issues

### Planned Features

**Future Enhancements:**
- 📊 Visualization of contribution distributions
- 📈 Interactive kinship heatmaps with zoom
- 🔄 Batch processing for multiple breeding scenarios
- 📋 Comparison of different OCS constraint strategies
- 💾 Save/load analysis sessions
- 📤 Additional export formats (CSV, JSON, PDF reports)

### Version History

**v2.0** (Current): Major UI overhaul, enhanced OCS options, WebR optimization
**v1.5**: Added custom OCS fallback, improved error handling
**v1.0**: Initial release with optiSel integration

## 📞 Support

For questions or issues:
1. Check the troubleshooting section above (16 common issues covered)
2. Click the "❓ Help" button in the app for complete documentation
3. Examine the R console output for detailed error messages
4. Review file status and startup guide for step-by-step diagnostics
5. Try the "📝 View R Code" tab to understand the underlying implementation

**For Developers:**
- Review the modular function organization in `app/R/`
- Check `scripts/validate_ocs_logic.R` for algorithm verification
- Use `scripts/compare_ocs_results.R` to compare implementations
- Examine `scripts/generate_fake_*.R` for creating test datasets

---

**AlloMate** - Making genetic breeding optimization accessible, robust, and efficient! 🧬✨

*Developed for aquaculture and livestock breeding programs. Optimizes genetic gain while controlling inbreeding through advanced contribution selection and mate allocation algorithms.*
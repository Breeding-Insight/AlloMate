# AlloMate - Genetic Breeding Optimization App

## 🧬 Overview

AlloMate is a Shiny web application for genetic breeding optimization using Optimum Contribution Selection (OCS). The app helps breeders make informed decisions about mate selection and breeding strategies by analyzing kinship relationships and breeding values.

## 🚀 Features

### Core Functionality
- **Kinship Analysis**: Calculate and visualize kinship relationships between potential mates
- **EBV Processing**: Combine multiple breeding value traits with user-defined weights
- **Optimum Contribution Selection**: Optimize breeding contributions while controlling inbreeding
- **Mating Plan Generation**: Create optimal mating pairs to minimize inbreeding
- **Excel Export**: Download comprehensive results in Excel format

### Technical Features
- **Dual Implementation**: Works with both optiSel package and custom fallback
- **WebR Compatible**: Runs in web browsers without server installation
- **Modular Design**: Clean, organized codebase for easy maintenance
- **Robust Error Handling**: Comprehensive validation and user feedback

## 📁 Project Structure

```
AlloMate/
├── app/                  # Shiny application
│   ├── global.R          # Shared code (packages, global variables)
│   ├── ui.R              # User interface code
│   ├── server.R          # Server logic and reactivity
│   ├── R/                # Helper functions and modules
│   │   ├── utils.R       # Data processing and utility functions
│   │   ├── ocs_helpers.R # OCS analysis functions
│   │   ├── ui_helpers.R  # UI helper functions
│   │   ├── load_functions.R # Function loader
│   │   └── optsel_fallback.R # Custom OCS fallback implementation
│   └── www/              # Static resources
│       ├── allomate.png
│       └── logos.png
├── scripts/              # Custom implementations and utilities
│   ├── optsel_fallback.R # Custom OCS fallback implementation
│   ├── validate_ocs_logic.R # OCS logic validation
│   ├── compare_ocs_results.R # Results comparison
│   ├── ped_ocs_app.R     # Pedigree OCS application
│   ├── dosage2gmat.R     # Dosage to genotype matrix conversion
│   └── test.r            # Testing utilities
├── data/                 # Sample data files
│   ├── candidates_2024_10_29.txt
│   ├── pedigree_with_family.txt
│   ├── weight_ebvs_for_app_with_family.txt
│   ├── length_ebvs_for_app_with_family.txt
│   └── shiny_output.txt  # Sample output data
├── AlloMate.Rproj        # RStudio project file
└── README.md
```

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
- **EBV Files**: Upload breeding value files with columns `ID` and `EBV`

### 2. Configure Analysis
- **Traits**: Add multiple EBV traits with relative weights (must sum to 1)
- **Kinship Threshold**: Set maximum allowed kinship between mates
- **OCS Parameters**: Set desired inbreeding rate and number of offspring

### 3. View Results
- **Kinship Matrix**: Visualize kinship relationships with color coding
- **EBV Matrix**: View combined breeding values for potential crosses
- **OCS Results**: See optimal candidate contributions and mating plan

### 4. Export Results
- Download comprehensive Excel files with all results
- Includes README sheets explaining the analysis

## 🔧 Technical Details

### OCS Implementation
The app uses a dual implementation approach:

#### Local Environment (optiSel Available)
1. App tries to install and load optiSel
2. If successful, uses native optiSel functions
3. Status shows: "✅ optiSel package is available - OCS functionality enabled"

#### WebR Environment (optiSel Not Available)
1. App detects optiSel installation failure
2. Automatically loads custom OCS fallback from `scripts/optsel_fallback.R`
3. Creates function aliases: `candes`, `opticont`, `noffspring`, `matings`
4. Status shows: "✅ Custom OCS fallback is available - OCS functionality enabled"

#### Complete Failure (Neither Available)
1. Shows error message: "❌ OCS functionality not available"
2. OCS button shows modal explaining the issue
3. User can still use EBV matrix functionality

### Data Processing
- Automatic pedigree cleaning and validation
- Kinship matrix calculation using kinship2
- Multi-trait EBV combination with weights
- Comprehensive error handling and validation

### Performance
- **Small datasets** (<100 candidates): Fast, comparable to optiSel
- **Medium datasets** (100-500 candidates): Reasonable performance
- **Large datasets** (>500 candidates): May be slower than optiSel



## 📈 Sample Results

With the provided sample data:
- **84 candidates** (63 males, 21 females)
- **66 candidates selected** for breeding
- **97 optimal mating pairs** generated
- **Mean offspring inbreeding**: 0.0053
- **Expected genetic gain**: +0.0557 (improved from -0.0290 baseline)

## 🔍 Troubleshooting

### Common Issues

1. **"Package not available" error**
   - The app will automatically use the custom fallback
   - Check console messages for details

2. **"No valid EBV files" error**
   - Ensure EBV files have correct column names (ID, EBV)
   - Check that trait weights sum to 1.0

3. **"Only one sex selected" error**
   - Adjust EBV scaling or kinship constraints
   - Check candidate file sex coding (M/F)

4. **"Optimization failed" error**
   - Check kinship matrix condition
   - Consider data scaling or constraint adjustment

5. **"Requested package not found in webR binary repo"**
   - This indicates the app is trying to use WebR installation methods
   - The app has been updated to handle this automatically

6. **"Permission denied"**
   - You don't have write access to the R library directory
   - Run R as administrator or change library location

### Debug Mode
Enable verbose output by checking the R console for detailed messages during analysis.

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

## 📞 Support

For questions or issues:
1. Check the troubleshooting section
2. Examine the console output for error details
3. Review the app documentation and help section

---

**AlloMate** - Making genetic breeding optimization accessible and efficient! 🧬✨
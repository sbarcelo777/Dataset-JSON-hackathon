<img src="www/json.svg" height="300" style="float:right; margin-left:10px;">


# JSON Dataset Viewer    

A powerful and user-friendly Shiny application for exploring and analyzing JSON datasets with interactive features and comprehensive data visualization capabilities.

## 🚀 Features

### Upload & Management

-   Support for JSON and NDJSON file formats
-   Multiple file upload capability
-   Automatic metadata display
-   File management system

### Data Exploration

-   Interactive data viewer with dynamic filtering
-   Advanced sorting capabilities
-   Column management (hide/show, sticky columns)
-   Row highlighting functionality

### Variable Analysis

-   Smart variable browser with automatic visualization
-   Grouping capabilities for deeper insights
-   Interactive outputs based on variable types
-   Statistical summaries and distributions

## 🛠️ Getting Started

### Prerequisites

This tool was built using R Version 4.4.1. Please try to use R version > 4. 

### Installation

First, download our repository. then run `setup.R`. This should install required packages and then  run the app. If the app is not running please try to run `app.R` script.

## 📖 User Guide

### File Upload Process

1.  Select JSON files through the browser
2.  Wait for complete upload (files appear in list)
3.  View metadata in the right panel
4.  Access the Viewer Panel once data is loaded

The JSON Dataset Viewer allows you to select json and ndjson files from a browser. You should be able to only select those supported extensions to avoid errors. Once the selected files are uploaded you are able to see the list of them in the card below, as well as the metadata behind it on the right side. You should only see the Viewer Panel appear when the data is fully loaded. Click on "Remove All Files" button to clear the list, this is basically reloading the session. It is not possible yet to delete one specific dataset from the list. Navigate across the radio buttons to see metadata from the different json files.

> ⚠️ **Important:** Be patient with large files and ensure complete upload before switching panels.

### Data Viewer Features

#### Viewer Left Box

The Viewer Left Box consists of three main panels:

1.  **Data Manipulation Panel**
    -   File selection
    -   R syntax filtering
    -   Download filtered data -**ONLY DATA FILTERED BY R SYNTAX**-
2.  **Variable Browser**
    -   Automatic visualization based on variable type
    -   Grouping functionality
    -   Interactive data exploration
3.  **Information Panel**
    -   Usage tips
    -   Filtering guidelines

#### 📝 Filtering Tips

For successful filtering operations:

| Operation | Syntax Example             |
|-----------|----------------------------|
| Equality  | `VAR == "Value"`           |
| AND       | `condition1 && condition2` |
| OR        | `condition1 II condition2` |
| NOT       | `!condition`               |
| Numeric   | `VARNUM < 78`              |

> 🔍 Remember to match variable names exactly and use double quotes for text values!

### Viewer Features

1.  **Dynamic filtering**
    -   Use a slider to filter numeric variables dynamically.
    -   Search for specific categorical values by typing into the search input located at the top of the corresponding column.
    -   These features leverage JavaScript to improve dataset navigation. Once you're satisfied with your filters, type in the Viewer Left Box to enable downloading the filtered data.
2.  **Column management**
    -   Click/unclick to show labels
    -   Select column to show or to stick
    -   Download R filtered data
3.  **Row highlighting functionality**
    -   Chose a column
    -   Select values (slider for numeric)
    -   Select a color

## Known Limitations

-   Bulk file removal only (individual file deletion coming soon)
-   Case-sensitive filtering system
-   Performance depends on environment capabilities
-   Row highlighting functionality still under development, please be sure to not hide the column selected for highlighting
-   Support NDJSON files coming soon

## 👥 Authors

-   **Sebastià Barceló**
-   **Hugo Signol**

## 📄 License

GPL License

------------------------------------------------------------------------

For more information or to report issues, please visit our GitHub repository.

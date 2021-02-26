
# CCtemplates

<!-- badges: start -->
<!-- badges: end -->

This R package provides ready-to-use HTML and PDF output formats and templates for RMarkdown documents, with a focus on generating standardized statistical reports for the Cleveland Clinic. 

## Formats gallery

The package provides HTML and PDF output formats. Examples of these will be added later.

### `html_report`

Adapted from the [bookdown](https://bookdown.org/) theme. Fully responsive with a dynamic table of contents and collapsible navigation. Also includes ability to export HTML as a formatted MS Word document using JavaScript.

### `pdf_report`

Generates a PDF document with branded header and subject block. 

## Installation

The latest version of the package can be downloaded from GitHub:

``` r
install.packages('remotes') # if necessary
remotes::install_github("dillonicus/CCtemplates")
```

## Starting a new project

To start a new project, simply use the `CCtemplates::project_create` function. This function takes various information about the project, generates a unique project ID, then creates a project directory and sub-folders containing pre-populated `rmarkdown`, R, and SAS files. A document entitled 'project_list.csv' will also be created in the main directory to track projects created with this function.

The function requires several arguments:  

- `project_folder`: Main project folder directory (where all projects are created/stored). Defaults to the default non-project directory in RStudio; and  
- `client_name`: Name of the client/PI of the project. Used to generate project ID.  

Additional arguments can be supplied to give more details about the project:  

- `client_email`: Email of the client/PI;  
- `statistician_name`: Name of the lead statistician;  
- `statistician_email`: Email of the lead statistician;  
- `programmer`: Name of the statistical programmer (if applicable);  
- `department`: Indicates which department the project is for. This will create a sub-folder within the main project directory if one does not already exist;  
- `project_title`: Optional title for the project;  
- `start_date`: Start date of the project. Default is today's date;  
- `version`: Version number of the project. Default is '1.0';  
- `notes`: Any general notes about the project; and  
- `output_format`: Changes the output format of the `rmarkdown` file created in the 'memo' folder. Options are 'html' (default) or 'pdf'.

``` r
CCtemplates::project_create(
  project_folder = "~/", 
  start_date = Sys.Date(), 
  client_name = "Client Name", 
  client_email = "client_name@ccf.org",
  statistician_name = "Dillon Corrigan", 
  statistician_email = "corrigand3@ccf.org", 
  programmer = "Programmer Name", 
  version = "1.0", 
  department = "GUKI", 
  project_title = "Test Project", 
  output_format = "html", 
  notes = "This is not a real project"
  )
```
## Removing a completed project

Projects can also be archived and/or deleted from the main project file using the `CCtemplates::project_remove` function.

The function requires several arguments:  

- `project_folder`: Main project folder directory (where all projects are created/stored). Defaults to the default non-project directory in RStudio; and  
- `project_id`: The ID of the project you wish to archive/remove. This is stored and looked up in the 'project_list.csv' file created by `CCtemplates::project_create()`.  

An additional argument, `keep_zip`, indicates whether you would like to archive the project prior to deletion. If set to `TRUE` (default), all project files will be added to a .zip archive and copied to a sub-folder of the main project folder directory called 'Completed.' If set to `FALSE`, no archive will be made and all files will be deleted. If set to `FALSE` the user will be prompted twice to confirm deletion without archive before proceeding.

```r
CCtemplates::project_remove(
  project_folder = "~/Projects",
  project_id = "D1234",
  keep_zip = TRUE
  )
```

## Creating a new document

Create a new `Rmd` file and add the following to the YAML preamble:

``` r
---
output: CCtemplates::<template name>
---
```

Within RStudio, you can also choose `File` > `New File...` > `R Markdown...` then select `From Template`. You should then be able to create a new document from one of the package templates.

## Options

Depending on output format, additional metadata and output options can be specified in the YAML preamble.

### Metadata

- `title`
- `date`
- `project`
- `version`
- `client`: PI for project. Name and email can be specified as list of entries under the `client` field.
- `statistician`: Statistician preparing report. Name and email can be specified as a list of entries under the `statistician` field.


### Output formatting

- `fig_width`: figures width, in inches
- `fig_height`: figures width, in inches
- `fig_caption`: toggle figure caption rendering
- `highlight`: syntax highlighting
- `thumbnails`: if TRUE, display content images as thumbnails
- `lightbox`: if TRUE, add lightbox effect to content images
- `gallery`: if TRUE, add navigation between images when displayed in lightbox
- `use_bookdown`: if TRUE, will use `bookdown` instead of `rmarkdown` for HTML rendering (adds section numbering and cross-referencing)

``` r
---
title: "Test Project"
date: "`r Sys.Date()`"
project: 
 - id: "D1234"
version: "1.0"
client: 
 - name: "Client Name"
   email: "client_name@ccf.org"
statistician: 
 - name: "Dillon Corrigan"
   email: "corrigd3@ccf.org"
output: CCtemplates::html_report
---
```

## Credits

- The CSS for `html_report` is directly derived from the [bookdown](https://bookdown.org) project template.
- JavaScript and HTML code for code folding and tabbed sections are taken from RStudio's default `rmarkdown` HTML template.
- The [rmdformats](https://github.com/juba/rmdformats) package served as a general reference for the `html_report` format.

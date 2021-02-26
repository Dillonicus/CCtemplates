
# CCtemplates

<!-- badges: start -->
<!-- badges: end -->

This R package provides ready-to-use $\LaTeX$ and HTML output formats and templates for RMarkdown documents, with a focus on generating standardized statistical reports for the Cleveland Clinic. 

## Formats gallery

The package provides $\LaTex$ and HTML output formats. Click on the images below for a sample output document.

### `html_report`

Adapted from the [bookdown](https://bookdown.org/) theme. Fully responsive with a dynamic table of contents and collapsible navigation. Also includes ability to export HTML as a formatted MS Word document using JavaScript.

## Installation

The latest version of the package can be downloaded from GitHub:

``` r
install.packages('remotes') # if necessary
remotes::install_github("dillonicus/CCtemplates")
```

## Starting a new project

``` r
CCtemplates::new_user()
```

``` r
CCtemplates::create_project
```
## Removing a completed project

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
 - id: "A1234"
version: "1.0"
client: 
 - name: "Mary Smith, M.D."
   email: "smith@ccf.org"
statistician: 
 - name: "Gary Stat"
   email: "stat@ccf.org"
output: CCtemplates::html_report
---
```

## Credits

- The CSS for `html_report` is directly derived from the [bookdown](https://bookdown.org) project template.
- JavaScript and HTML code for code folding and tabbed sections are taken from RStudio's default `rmarkdown` HTML template.
- The [rmdformats](https://github.com/juba/rmdformats) package served as a general reference for the `html_report` format.

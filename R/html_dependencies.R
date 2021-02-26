# readthedown js and css
html_dependency_cc <- function(embed_fonts = TRUE) {
  stylesheets <- "html_report.css"
  if (embed_fonts) {
    stylesheets <- c(stylesheets, "html_report_fonts.css")
  } else {
    stylesheets <- c(stylesheets, "html_report_fonts_download.css")
  }
  htmltools::htmlDependency(name = "html_report",
                            version = "0.1",
                            src = system.file("html_resources/html_report", package = "CCtemplates"),
                            script = "html_report.js",
                            stylesheet = stylesheets)
}

# create an html dependency for Magnific popup
html_dependency_magnific_popup <- function() {
  htmltools::htmlDependency(name = "magnific-popup",
                            version = "1.1.0",
                            src = system.file("html_resources/magnific-popup-1.1.0", package = "CCtemplates"),
                            script = "jquery.magnific-popup.min.js",
                            stylesheet = "magnific-popup.css")
}

# create an html dependency for jquery-stickytableheaders
html_dependency_jquery_stickytableheaders <- function()  {
  htmltools::htmlDependency(name = "jquery-stickytableheaders",
                            version = "0.1.11",
                            src = system.file("html_resources/jquery-stickytableheaders-0.1.11", package = "CCtemplates"),
                            script = "jquery.stickytableheaders.min.js")
}


# create an html dependency for bootstrap (function copied from rmarkdown)
html_dependency_bootstrap <- function(theme = "bootstrap") {
  htmltools::htmlDependency(name = "bootstrap",
                 version = "3.3.7",
                 src = system.file("html_resources/bootstrap-3.3.7", package = "CCtemplates"),
                 meta = list(viewport = "width=device-width, initial-scale=1"),
                 script = c(
                   "js/bootstrap.min.js"
                   # These shims are necessary for IE 8 compatibility
                   #"shim/html5shiv.min.js",
                   #"shim/respond.min.js"
                 ),
                 stylesheet = paste("css/", theme, ".min.css", sep = ""))
}

# create an html dependency for bootstrap js only (function copied from rmarkdown)
html_dependency_bootstrap_js <- function() {
  htmltools::htmlDependency(name = "bootstrap_js",
                            version = "3.3.7",
                            src = system.file("html_resources/bootstrap-3.3.7", package = "CCtemplates"),
                            meta = list(viewport = "width=device-width, initial-scale=1"),
                            script = c(
                              "js/bootstrap.min.js"
                            ))
}

# Mathjax (functions copied from rmarkdown)
default_mathjax <- function() {
  paste0("https://mathjax.rstudio.com/latest/", mathjax_config())
}
mathjax_config <- function() {
  "MathJax.js?config=TeX-AMS-MML_HTMLorMML"
}

# Navigation.js (copied and adapted from rmarkdown
# because it is not exported)
html_dependency_navigation <- function(code_menu = TRUE, source_embed = FALSE) {
  # dynamically build script list
  script <- c("tabsets.js")
  if (code_menu)
    script <- c(script, "codefolding.js")
  if (source_embed)
    script <- c(script, "FileSaver.min.js", "sourceembed.js")
    htmltools::htmlDependency(name = "navigation",
                              version = "1.1",
                              src = system.file("html_resources/navigation-1.1", package = "CCtemplates"),
                              script = script)
}

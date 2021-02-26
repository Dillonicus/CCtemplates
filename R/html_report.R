# HTML -------------------------------------------------------------------------

#' Convert to an HTML document
#'
#' Format for converting from R Markdown to an HTML document.
#'
#' @details
#' CSS adapted from the bookdown project.
#'
#' @param fig_width Default width (in inches) for figures
#' @param fig_height Default width (in inches) for figures
#' @param fig_caption \code{TRUE} to render figures with captions
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", "haddock", and "textmate". Pass \code{NULL} to prevent syntax
#'   highlighting.
#' @param lightbox if TRUE, add lightbox effect to content images
#' @param thumbnails if TRUE display content images as thumbnails
#' @param gallery if TRUE and lightbox is TRUE, add a gallery navigation between images in lightbox display
#' @param pandoc_args arguments passed to the pandoc_args argument of rmarkdown \code{\link[rmarkdown]{html_document}}
#' @param md_extensions arguments passed to the md_extensions argument of rmarkdown \code{\link[rmarkdown]{html_document}}
#' @param toc_depth adjust table of contents depth
#' @param embed_fonts if TRUE, use local files for fonts used in the template. This leads to bigger files but ensures that these fonts are available.
#' @param use_bookdown if TRUE, uses \code{\link[bookdown]{html_document2}} instead of \code{\link[rmarkdown]{html_document}}, thus providing numbered sections and cross references
#' @param mathjax set to NULL to disable Mathjax insertion
#' @param ... Additional function arguments passed to R Markdown \code{\link[rmarkdown]{html_document}}
#' @return R Markdown output format to pass to \code{\link[rmarkdown]{render}}
#' @import rmarkdown
#' @import bookdown
#' @importFrom htmltools htmlDependency
#' @export

html_report <- function(fig_width = 8,
                        fig_height = 5,
                        fig_caption = TRUE,
                        highlight = "kate",
                        lightbox = FALSE,
                        thumbnails = FALSE,
                        gallery = FALSE,
                        toc_depth = 3,
                        embed_fonts = FALSE,
                        use_bookdown = FALSE,
                        source_embed = TRUE,
                        code_folding = "hide",
                        link_citations = TRUE,
                        pandoc_args = NULL,
                        md_extensions = NULL,
                        mathjax = "CCtemplates",
                        ...) {

  html_report_format(
    template_name = "html_report",
    template_path = "html_resources/html_report.html",
    template_dependencies = list(
      html_dependency_cc(embed_fonts)
    ),
    pandoc_args = pandoc_args,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_caption = fig_caption,
    highlight = highlight,
    lightbox = lightbox,
    thumbnails = thumbnails,
    gallery = gallery,
    toc = TRUE,
    toc_depth = toc_depth,
    source_embed = source_embed,
    code_folding = code_folding,
    link_citations = link_citations,
    use_bookdown = use_bookdown,
    md_extensions = md_extensions,
    mathjax = mathjax,
    ...
  )

}

#' Function that accepts arguments and formats for passing to rmarkdown/bookdown build functions.
#' @param template_name name of the template
#' @param template_path path to the html template, relative to the package 'inst' folder
#' @param template_dependencies list containing an html_dependency_* function call
#' that inserts desired .css and .js files
#' @param pandoc_args character vector containing pandoc output options
#' @param ... additional arguments to be passed to the rmarkdown::html_document
#' or bookdown::html_document2 function
html_report_format <- function(
  template_name,
  template_path,
  template_dependencies,
  pandoc_args,
  ...) {

  args <- list(...)
  code_folding <- args[["code_folding"]]
  code_folding <- ifelse(is.null(code_folding), "none", code_folding)
  code_download <- args[["code_download"]]
  code_download <- ifelse(is.null(code_download), FALSE, code_download)
  code_menu <- !identical(code_folding, "none") || code_download

  ## js and css dependencies
  extra_dependencies <- c(
    list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_jqueryui(),
      html_dependency_navigation(
        code_menu = code_menu,
        source_embed = code_download
      ),
      html_dependency_bootstrap("bootstrap"),
      html_dependency_magnific_popup()
    ),
    template_dependencies
  )
  ## Merge "extra_dependencies"
  if ("extra_dependencies" %in% names(args)) {
    extra_dependencies <- append(extra_dependencies, args[["extra_dependencies"]])
    args[["extra_dependencies"]] <- NULL
    args[["mathjax"]] <- NULL
  }

  ## Force mathjax arguments
  if (!is.null(args[["mathjax"]])) {
    pandoc_args <- c(pandoc_args,
                     "--mathjax",
                     "--variable", paste0("mathjax-url:", default_mathjax()))
  }
  ## Other arguments
  pandoc_args <- c(pandoc_args,
                   "--variable", paste0(template_name, ":true"))
  if (args[["lightbox"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "lightbox:true")
  }
  if (args[["thumbnails"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "thumbnails:true")
  }
  if (args[["link_citations"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "link-citations:true")
  }
  if (args[["gallery"]]) {
    pandoc_args <- c(pandoc_args, "--variable", "gallery:true")
  } else {
    pandoc_args <- c(pandoc_args, "--variable", "gallery:false")
  }
  if (!is.null(args[["cards"]])) {
    if (args[["cards"]]) {
      pandoc_args <- c(pandoc_args, "--variable", "cards:true")
    }
  }

  ## Call rmarkdown::html_document
  html_document_args <- list(
    template = system.file(template_path, package = "CCtemplates"),
    extra_dependencies = extra_dependencies,
    pandoc_args = pandoc_args
  )
  html_document_args <- append(html_document_args, args)
  if (args[["use_bookdown"]]) {
    html_document_func <- bookdown::html_document2
  } else {
    html_document_func <- rmarkdown::html_document
  }
  do.call(html_document_func, html_document_args)
}

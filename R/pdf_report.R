
default_pandoc_args <- c("")

pdf_report_format <- function(
  format, template = find_resource(format, 'template.tex'), pandoc_args = default_pandoc_args, ...
) {
  fmt <- rmarkdown::pdf_document(pandoc_args = pandoc_args, template = template, latex_engine = "xelatex", ...)
  fmt$inherits <- "pdf_document"
  fmt
}

#'
#' @export
pdf_report <- function(..., keep_tex = FALSE) {
  pdf_report_format(
    format = 'pdf_report', keep_tex = keep_tex, ...
  )
}

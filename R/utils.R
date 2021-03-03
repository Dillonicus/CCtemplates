#' Function to create new project directory if it doesn't already exist.
#'
#' @param directory character vector designating the path to the new project directory
#'
create_proj_dir <- function(directory){
  if(fs::dir_exists(directory)){
    stop("Directory already exists. Please specify a new project directory")
  } else{
    fs::dir_create(directory)
  }
}

#' Function to generate unique project id
#'
#' @param name character vector of which the first letter will be used in conjuction with a randomly sampled number to create a unique id.
#' @param existing_ids vector of existing project ids taken from
proj_id <- function(name, existing_ids) {
  id <- paste0(substring(name, 1, 1), sample(c(1000:3000), size = 1))
  while(id %in% existing_ids) {
    id <- paste0(substring(name, 1, 1), sample(c(1000:3000), size = 1))
  }
  id
}

#' Function to generate YAML header in project files
#'
yaml_maker <- function(start_date = '`r format(Sys.time(), "%Y/%m/%d")`',
                       version = "1.0",
                       dept = "GUKI",
                       statistician_name = NULL,
                       statistician_email = NULL,
                       client_name = NULL,
                       client_email = NULL,
                       project_id = NULL,
                       project_title = NULL,
                       output_format = "html_report",
                       ...){
  arglist <- list(
    date = list(date = start_date),
    version = list(version = version),
    dept = list(dept = dept),
    statistician = list(
      name = statistician_name,
      email = statistician_email),
    client = list(
      name = client_name,
      email = client_email),
    project = list(
      id = project_id,
      title = project_title),
    ...
  )

  yamlr <- function(listarg){
    no_null <- sapply(listarg, function(y) y[!sapply(y, is.null)])
    categ <- names(no_null)
    # out <- purrr::map(
    #   .x = categ,
    #   .f = ~{
    #     sub <- no_null[[.x]]
    #     if(length(sub) > 1){
    #       header <- glue::glue("{.x}: ")
    #       content <- sapply(
    #         1:length(sub),
    #         function(y) glue::glue("  {names(sub)[[y]]}: \'{sub[[y]]}\'")
    #       )
    #       content[1] <- sub(pattern = " ", replacement = "-", x = content[1])
    #       c(header, content)
    #     } else{
    #       glue::glue("{.x}: \'{sub}\'")
    #     }
    #   }
    # )
    #
    out <- lapply(
      X = categ,
      FUN = function(x) {
        sub <- no_null[[x]]
        if(length(sub) > 1) {
          header <- glue::glue("{x}: ")
          content <- sapply(
            1:length(sub),
            function(y) glue::glue("  {names(sub)[[y]]}: \'{sub[[y]]}\'")
            )
          content[1] <- sub(pattern = " ", replacement = "-", x = content[1])
          c(header, content)
        } else {
          glue::glue("{x}: \'{sub}\'")
        }
      })

    c("---",
      unlist(out),
      glue::glue("output: CCtemplates::{output_format}"),
      "---")
  }
  yamlr(arglist)
}

#' Function to draft files for project
#' @import rmarkdown
draft_cc <- function(file, template, metadata, package = NULL, create_dir, edit = FALSE){
  if (!is.null(package)) {
    template_path = system.file(
      "rmarkdown", "templates", template, package = package
    )
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ",
           package, " package")
    }
  } else {
    template_path <- template
  }

  template_yaml <- file.path(template_path, "template.yaml")

  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", template, "'")
  }

  template_meta <- rmarkdown:::yaml_load_file(template_yaml)
  if (is.null(template_meta$name) || is.null(template_meta$description)) {
    stop("template.yaml must contain name and description fields")
  }

  if (identical(create_dir, "default"))
    create_dir <- isTRUE(template_meta$create_dir)

  if (create_dir) {
    file <- tools::file_path_sans_ext(file)
    if (fs::dir_exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file)
    file <- file.path(file, basename(file))
  }

  if (!identical(tolower(tools::file_ext(file)), "rmd"))
    file <- paste(file, ".Rmd", sep = "")
  if (file.exists(file))
    stop("The file '", file, "' already exists.")
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  to <- dirname(file)

  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f))))
      stop("The file '", basename(f), "' already exists")
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }

  file.rename(file.path(dirname(file), "skeleton.Rmd"),
              file)

  if(!is.null(metadata)){
    rmd_raw <- rmarkdown:::partition_yaml_front_matter(xfun::read_utf8(normalizePath(file)))
    rmd_raw$front_matter <- metadata
    rmd_raw <- c(rmd_raw$front_matter, rmd_raw$body)

    xfun::write_utf8(text = rmd_raw, con = normalizePath(file))
  }

  if (edit)
    utils::file.edit(normalizePath(file))
  invisible(file)
}


pkg_file <- function(...) system.file(..., package = "CCtemplates")

find_resource <- function(template, file) {
  res <- pkg_file("rmarkdown", "templates", template, "resources", file)
  if (res == "") stop(
    "Couldn't find template file ", template, "/resources/", file, call. = FALSE
  )
  res
}

null_transformer <- function(str = "NULL") {
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (is.null(out)) {
      return(str)
    }
    out
  }
}

glue_null <- function(
  ...,
  .sep = "",
  .envir = parent.frame(),
  .open = "{", .close = "}",
  .na = "NA",
  .transformer = null_transformer(),
  .trim = TRUE) {
  glue::glue(..., .sep = .sep, .envir = .envir, .open = .open, .close = .close,
             .na = .na, .transformer = .transformer, .trim = .trim)
}

headers <- list(
  "R/{id}_v{version}.R" =
    "#*******************************************************************************
   #          Project: {id}
   #       Start Date: {start_date}
   #          Version: {version}
   #               PI: {first_last$first} {first_last$last}
   #       Department: {department}
   #     Statistician: {statistician_name}
   #  Stat Programmer: {programmer}
   #             Path: {file.path(new_proj_dir, 'R')}
   #            Notes: {notes}
   #*******************************************************************************

   ################################################################################
   ## Start-Up Information
   ## Mapped Drives: {file.path(project_folder)}
   ################################################################################"
  ,
  "SAS/{id}_v{version}.sas" =
    '/****************************************************************************
      *   Project Number: {id}
      *       Start date: {start_date}
      *          Version: {version}
      *           Client: {first_last$first} {first_last$last}
      *    Client Depart: {department}
      *     Statistician: {statistician_name}
      *  Stat Programmer: {programmer}
      *             Path: {file.path(new_proj_dir, "SAS")}
      *            Notes: {notes}
      ****************************************************************************/',

  "{id}.Rproj" = '
  Version: 1.0
  RestoreWorkspace: No
  SaveWorkspace: No
  AlwaysSaveHistory: Yes

  EnableCodeIndexing: Yes
  UseSpacesForTab: Yes
  NumSpacesForTab: 2
  Encoding: UTF-8

  RnwWeave: knitr
  LaTeX: XeLaTeX

  AutoAppendNewline: Yes
  StripTrailingWhitespace: Yes'
)


#' Function to create a new project.
#' @param project_folder Directory where project files are stored (usually a network drive location).
#' @param start_date Start date of project. Defaults to today's date.
#' @param client_name Name of PI/Client.
#' @param client_email Email of PI/Client.
#' @param statistician_name Name of lead statistician.
#' @param statistician_email Email of lead statistician.
#' @param version Optional version number. Defaults to version 1.0.
#' @param department PI/Client department affiliation.
#' @param project_title Optional title/description of project.
#' @param output_format Sets the output format for statistical reports. Options include
#'        "html" (default) or "pdf." Output format can be
#'        changed in YAML header of Rmarkdown document.
#' @import data.table
#' @export
project_create <- function(
  project_folder = "~/",
  start_date = Sys.Date(),
  client_name = NULL,
  client_email = NULL,
  statistician_name = NULL,
  statistician_email = NULL,
  programmer = NULL,
  version = "1.0",
  department = NULL,
  project_title = NULL,
  output_format = "html",
  notes = ""
  ){

  out_fmt <- list(
    pdf = "pdf_report",
    html = "html_report")
  template <- out_fmt[[output_format]]

  if(!file.exists(glue::glue("{project_folder}/project_list.csv"))) {
    file <- data.table::data.table(
      project = c(""),
      pi = c(""),
      statistician = c(""),
      start_date = c(""),
      status = c(""),
      status_date = c(""),
      complete = c(""),
      new_proj_dir = c("")
    )
    data.table::fwrite(x = file, file = glue::glue("{project_folder}/project_list.csv"))
  }
  projects <- data.table::fread(glue::glue("{project_folder}/project_list.csv"))
  first_last <- list(
    "first" = ifelse(
      grepl(x = client_name, ","),
      stringr::str_split(client_name, ",")[[1]][2],
      stringr::str_split(client_name, "\\s")[[1]][1]),
    "last" = ifelse(
      grepl(x = client_name, ","),
      stringr::str_split(client_name, ",")[[1]][1],
      stringr::str_split(client_name, "\\s")[[1]][2])
    )
  id <- proj_id(name = first_last[["last"]], existing_ids = projects[["project"]])
  new_proj_dir <- glue::glue("{project_folder}/{department}/{first_last[['last']]}/{id}")
  create_proj_dir(new_proj_dir)

  proj_meta <- yaml_maker(
    start_date = glue::glue("{start_date}"),
    client_name = client_name,
    client_email = client_email,
    statistician_name = statistician_name,
    statistician_email = statistician_email,
    project_id = id,
    project_title = project_title,
    output_format = output_format)

  headers <- lapply(
    headers,
    function(x) glue::glue(x, .transformer = null_transformer())
    )

  names(headers) <- sapply(
    USE.NAMES = F,
    simplify = T,
    names(headers),
    function(x) glue::glue(x, .transformer = null_transformer())
    )

  proj_files <- file.path(new_proj_dir, names(headers))
  names(headers) <- proj_files

  subdirs <- file.path(
    new_proj_dir,
    c("communications", "data", "graphs", "memo",
      "orig_data", "other", "papers", "R", "SAS", "temp"))

  fs::dir_create(subdirs)

  draft_cc(
    file = glue::glue("{new_proj_dir}/memo/{id}_v{version}.Rmd"),
    metadata = proj_meta,
    template = template,
    package = "CCtemplates",
    edit = F,
    create_dir = T)

  for(fname in proj_files){
    if(!fs::file_exists(fname)) {
    cat(headers[[fname]], file = fname)
    }
  }

  newdat <- data.table::data.table(
    project = id,
    pi = first_last[["last"]],
    statistician = statistician_name,
    start_date = c(glue::glue("{start_date}")),
    status = c(""),
    status_date = c(""),
    complete = c(""),
    proj_dir = path.expand(new_proj_dir))

  projects <- rbind(projects, newdat, fill = TRUE)
  data.table::fwrite(projects, glue::glue("{project_folder}/project_list.csv"))

  message(glue::glue("Project {id} successfully created at {new_proj_dir}."))
}

#' @export
project_remove <- function(project_folder = "~/", project_id, keep_zip = TRUE){

  if (keep_zip != TRUE) {
    message("CAUTION! Setting `keep_zip` to FALSE")
    message("will remove all project files without")
    message("making a backup.")
    confirm <- readline(prompt = "Do you wish to proceed? (Y/N)")
    if(confirm != "Y") stop(glue::glue("Project {project_id} has not been deleted."))
  }

  projects <- data.table::fread(glue::glue("{project_folder}/project_list.csv"))
  proj_path <- projects[project == project_id, proj_dir]

  if(!fs::dir_exists(glue::glue("{path.expand(project_folder)}/Completed"))) {
    fs::dir_create(glue::glue("{path.expand(project_folder)}/Completed"))
  }

  zipfile <- c(glue::glue("{path.expand(project_folder)}/Completed/{project_id}.zip"))
  files <- c(glue::glue("./{list.files(proj_path)}"))
  zip::zip(
    zipfile = zipfile,
    files = files,
    root = proj_path)

  message(
    glue::glue("Project {project_id} successfully archived at {project_folder}/Completed/{project_id}.zip")
    )

  fs::dir_delete(proj_path)

  message(
    glue::glue("All files in {proj_path} have been deleted.")
  )
}

#' Function to create a new user file that will be used as defaults when creating a new project.
#' @param project_dir Character vector that defines where
new_user <- function(project_dir = NULL, name = NULL, email = NULL) {
  id.file <- glue::glue("{project_dir}/{name}_id.txt")
  if(file.exists(glue::glue(id.file))) {
    stop(glue::glue("ID file '{name}_id.txt' already exists!"))
  }
  out <- data.frame(
    statistician = name)
  write.table(file = id.file)
}





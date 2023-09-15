#' Fetch Branch at the Object Level
#'
#' This builds a data frame containing the history of all objects of a package
#' on a given branch.
#'
#' @param repo Path or url to a git repo.
#' @param branch Name of the branch of interest.
#'
#' @export
git_history <- function(repo = ".", branch = "main") {
  if (startsWith(repo, "https")) {
    tmp <- tempfile()
    dir.create(tmp)
    gert::git_clone(repo, tmp)
    repo <- tmp
    message(sprintf("Created local repo at: %s", tmp))
  }

  #-----------------------------------------------------------------------------
  # get commit history + meta data
  log_cmd <- sprintf("git -C '%s' log %s --pretty=format:'%%h,%%ae,%%cI,%%s'", repo, branch)
  commits <- system(log_cmd, intern = TRUE)

  #-----------------------------------------------------------------------------
  # wrangle into data.frame, taking into account that commas in commit title are not separators
  message("Wrangle commits")
  commits_df <- purrr::map_dfr(
    strsplit(commits, ","),
    ~ tibble::tibble(
      commit = .x[[1]],
      author = .x[[2]],
      time = .x[[3]] |>
        lubridate::parse_date_time2("%Y-%m-%dT%H:%M:%S%OO", exact = TRUE),
      description = paste(.x[-(1:3)], collapse = ","),
    ),
    .progress = TRUE
  )

  #-----------------------------------------------------------------------------
  # Fetch affected files for every commit
  get_affected_files <- function(commit) {
    cmd <- sprintf("git -C '%s' show --name-only --pretty=format: %s", repo, commit)
    files <- system(cmd, intern = TRUE)
    # keep only R files from the R folder
    files <- grep("R/.*[.](R|r)$", files, value = TRUE)
    files
  }

  message("Collect changed files")
  files_df <-
    commits_df |>
    dplyr::mutate(file = purrr::map(commit, get_affected_files, .progress = TRUE)) |>
    tidyr::unnest_longer(file)

  #-----------------------------------------------------------------------------
  # Fetch objects for every affected file
  parse_commit_file  <- function(commit, file) {
    cmd <- sprintf("git -C '%s' show %s:'%s'", repo, commit, file)
    file_code <- suppressWarnings(tryCatch(
      system(cmd, intern = TRUE, ignore.stderr = TRUE),
      error = function(e) "REMOVED"
    ))
    parsed_data <- try(getParseData(parse(text = file_code, keep.source = TRUE), includeText = TRUE), silent = TRUE)
    # non syntactic edits (e.g. containing conflict markers) are skipped
    if (inherits(parsed_data, "try-error")) return(NULL)
    assignment_ids <- parsed_data$parent[parsed_data$token == "LEFT_ASSIGN"]
    top_level_assignment_ids <- parsed_data$id[parsed_data$id %in% assignment_ids & parsed_data$parent == 0]
    if (!length(top_level_assignment_ids)) return(NULL)
    obj_names <- sapply(top_level_assignment_ids, function(x) parsed_data$text[which(parsed_data$parent == x)[[1]]])
    parsed_data$id <- ifelse(parsed_data$parent < 0, abs(parsed_data$parent), parsed_data$id)
    parsed_data <- parsed_data[parsed_data$id %in% top_level_assignment_ids, c("id", "text")]
    code <- tapply(parsed_data$text, parsed_data$id, function(x) paste(x, collapse = "\n"))
    file_data <- data.frame(object = obj_names, code = code)
    file_data
  }

  rleid <- function(x) {
    x <- rle(x)$lengths
    rep(seq_along(x), times=x)
  }

  message("Parse files")
  objects_df <-
    files_df |>
    dplyr::mutate(object = purrr::pmap(list(commit, file), parse_commit_file, .progress = TRUE)) |>
    tidyr::unnest(object)

  #-----------------------------------------------------------------------------
  # Remove redundancies and format into proper history df
  history <-
    objects_df |>
    # add NA code and file for every combination of commit and object
    tidyr::complete(tidyr::nesting(commit, author, time, description), object) |>
    # arrange chronologically to prepare for rleid() below
    dplyr::arrange(time) |>
    dplyr::group_by(object) |>
    # fill these NAs to reflect actual status at every step
    tidyr::fill(code, file) |>
    # identify running length sequences (spans in which object didn't change)
    dplyr::mutate(id = rleid(paste(file, code))) |>
    dplyr::ungroup() |>
    # keep only 1st and last during unchanged span
    dplyr::slice(
      .by = c(object, id),
      unique(c(1, dplyr::n()))
    ) |>
    # replace file and code with intuitive values for diffobj in review_object_history()
    tidyr::replace_na(list(file = "none", code = "")) |>
    dplyr::select(-id)

  history
}

#' Review object history
#'
#' On the left (resp. right) we find the latest (resp. earliest) commit for a given object.
#'
#' If the object was moved from a file to another it is tracked, we also detect if
#' the object was removed/recreated.
#'
#' @param git_history output of git_history()
#'
#' @param object The name of an object.
#' @param ascending A boolean.
#' @param mode,context Passed to `diffobj::diffChr()`, with different defaults.
#'
#' @export
review_object_history <- function(
    git_history,
    object,
    ascending = TRUE,
    mode = c("sidebyside", "unified", "context", "auto"),
    context = -1
    ) {
  mode <- match.arg(mode)
  diff_data <- git_history |>
    dplyr::arrange(time) |>
    dplyr::filter(object == .env$object) |>
    dplyr::transmute(
      banner = sprintf("%s<br>%s<br>%s<br>%s<br>%s", time, commit, author, description, file),
      file,
      code = strsplit(code, "\n")
    )

  if (ascending) {
    i <- 0
  } else {
    i <- nrow(diff_data)
  }
  tmp <- tempfile(fileext = ".html")
  press <- ""
  repeat {
    step <- prod(ifelse(c(press == "", ascending), 1, -1))
    i <- i + step
    if (i %in% c(0, nrow(diff_data))) break
    break_ <- FALSE
    while(identical(diff_data$code[[i+1]], diff_data$code[[i]]) &&
          identical(diff_data$file[[i+1]], diff_data$file[[i]])) {
      i <- i + step
      if (i %in% c(0, nrow(diff_data))) {
        break_ <- TRUE
        break
      }
    }
    if (break_) break

    x <- diffobj::diffChr(
      diff_data$code[[i]],
      diff_data$code[[i+1]],
      mode = mode,
      tar.banner = paste("older:", diff_data$banner[[i]]),
      cur.banner = paste("newer:", diff_data$banner[[i+1]]),
      ignore.white.space = FALSE,
      convert.hz.white.space = FALSE,
      strip.sgr = FALSE,
      trim = FALSE,
      context = context,
      pager = list(file.path = tmp)
    )
    print(x)
    press <- readline("Press ENTER to continue, or any other key then ENTER to go back: ")
  }
}

globalVariables(c("object", "author", "time", "description", "code", "id", ".env", "commit"))


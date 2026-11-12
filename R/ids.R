# Drawing IDs and file names

#' Safe file name stem from an ID; never use the raw ID as a path
pd_file_stem <- function(id) {
  stem <- gsub("[^A-Za-z0-9._-]", "_", id)
  sub("^\\.", "_", stem)
}

#' Check a drawing ID
#'
#' @param id Raw text from the ID input.
#' @param used Named integer vector: names are IDs already downloaded in this
#'   session, values the drawing number they were used for.
#' @param drawing Number of the current drawing. Re-using an ID for the same
#'   drawing (downloading it again) is allowed.
#' @return list(ok, id (trimmed), message)
pd_check_id <- function(
  id,
  used = integer(),
  drawing = 1L,
  max_chars = pd_config$max_id_chars
) {
  id <- if (pd_is_string(id)) trimws(id) else ""
  fail <- function(msg) list(ok = FALSE, id = id, message = msg)
  n <- nchar(id, type = "chars", allowNA = TRUE)
  if (is.na(n)) {
    return(fail(pd_tr("id_invalid")))
  }
  if (n == 0) {
    return(fail(pd_tr("id_empty")))
  }
  if (n > max_chars) {
    return(fail(pd_tr("id_too_long", max_chars)))
  }

  others <- names(used)[used != drawing]
  if (id %in% others) {
    return(fail(pd_tr("id_used", id)))
  }
  clash <- others[pd_file_stem(others) == pd_file_stem(id)]
  if (length(clash) > 0) {
    return(fail(pd_tr("id_file_clash", id, clash[1])))
  }

  list(ok = TRUE, id = id, message = NULL)
}

#' Suggest the ID for the next drawing
#'
#' Keeps `current` if it is unused; otherwise increments its trailing number,
#' keeping zero padding ("P009" -> "P010"), or appends "_2".
pd_next_id <- function(current, used = character()) {
  current <- if (pd_is_string(current)) trimws(current) else ""
  if (!nzchar(current)) {
    current <- "1"
  }
  if (!current %in% used) {
    return(current)
  }

  m <- regmatches(current, regexec("^(.*?)([0-9]+)$", current))[[1]]
  if (length(m) == 3) {
    prefix <- m[2]
    num <- as.numeric(m[3])
    width <- nchar(m[3])
  } else {
    prefix <- paste0(current, "_")
    num <- 1
    width <- 1
  }
  repeat {
    num <- num + 1
    candidate <- paste0(
      prefix,
      formatC(num, width = width, flag = "0", format = "d")
    )
    if (!candidate %in% used) return(candidate)
  }
}

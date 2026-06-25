# Variable inspector for the R (IRkernel) Jupyter kernel.
#
# Everything is wrapped in a single object, `.codedown_variable_inspector`, so the
# inspector adds exactly one name to the global environment. The leading dot keeps
# it out of ls() (so it's hidden from its own listing). The frontend runs this file
# once at kernel startup, then calls `.codedown_variable_inspector$dict_list()` and
# `.codedown_variable_inspector$inspect('<name>')`.
#
# jsonlite is always available (a propagated dependency of IRkernel). It drops named
# list elements whose value is NULL, so we use NA together with na = "null" to emit a
# real JSON null while keeping the key.

.codedown_variable_inspector <- local({
  max_content <- 150L
  max_rows <- 10000L

  typ <- function(x) {
    cls <- class(x)
    if (length(cls) == 0) "NULL" else paste(cls, collapse = "/")
  }

  sz <- function(x) {
    s <- tryCatch(as.numeric(object.size(x)), error = function(e) NA_real_)
    if (is.na(s)) NA_integer_ else as.integer(s)
  }

  ismat <- function(x) {
    is.data.frame(x) || is.matrix(x) || (is.array(x) && length(dim(x)) == 2L)
  }

  shp <- function(x) {
    # Wrap in I() so jsonlite renders a JSON array even for a single dimension
    # (auto_unbox would otherwise collapse a length-1 vector to a scalar).
    d <- dim(x)
    if (!is.null(d)) return(I(as.integer(d)))
    if (is.vector(x) || is.list(x) || is.factor(x)) return(I(as.integer(length(x))))
    NA_integer_
  }

  trunc_str <- function(s, n) {
    s <- gsub("[\r\n]+", " ", paste(s, collapse = " "))
    if (nchar(s) > n) paste0(substr(s, 1L, n), " ...") else s
  }

  cont <- function(x) {
    preview <- tryCatch(
      if (is.data.frame(x)) {
        paste0("Columns: ", paste(names(x), collapse = ", "))
      } else {
        paste(utils::capture.output(utils::str(x, max.level = 1L, list.len = 10L, vec.len = 10L)), collapse = " ")
      },
      error = function(e) tryCatch(paste(format(x), collapse = " "), error = function(e2) "<unprintable>")
    )
    trunc_str(preview, max_content)
  }

  user_vars <- function() {
    nms <- ls(envir = globalenv(), all.names = FALSE)
    nms[!startsWith(nms, ".codedown_variable_inspector")]
  }

  tbl <- function(x) {
    df <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = FALSE)
    if (nrow(df) > max_rows) df <- df[seq_len(max_rows), , drop = FALSE]
    list(
      columns = as.list(names(df)),
      data = unname(lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, ]))))
    )
  }

  dict_list <- function() {
    out <- list()
    for (nm in user_vars()) {
      v <- tryCatch(get(nm, envir = globalenv()), error = function(e) NULL)
      out[[nm]] <- list(
        type = typ(v),
        size = sz(v),
        shape = shp(v),
        content = cont(v),
        isMatrix = ismat(v)
      )
    }
    cat(jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", force = TRUE))
    cat("\n")
    invisible(NULL)
  }

  inspect <- function(name) {
    v <- tryCatch(get(name, envir = globalenv()), error = function(e) NULL)
    m <- ismat(v)
    full <- tryCatch(paste(utils::capture.output(print(v)), collapse = "\n"), error = function(e) "<unprintable>")
    out <- list(
      name = name,
      type = typ(v),
      size = sz(v),
      shape = shp(v),
      isMatrix = m,
      content = full,
      table = if (m) tbl(v) else NA
    )
    cat(jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", force = TRUE))
    cat("\n")
    invisible(NULL)
  }

  environment()
})

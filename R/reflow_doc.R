#' Reflow (reindent to margin) Rmarkdown document
#'
#' @param case_from Where to cast from
#' @export
reflow_doc <- function(cast_from = ".") {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("missing 'rstudioapi' pkg")
  }
  context_fun <- tryCatch(getFromNamespace("getSourceEditorContext",
      "rstudioapi"), error = function(e) rstudioapi::getActiveDocumentContext)
  doc <- context_fun()[["path"]]
  if (is.null(doc))
    stop("Cannot find the current active document in RStudio")
  if (doc == "")
    stop("Please save the current document first")
  if (!grepl("\\.rm(d|arkdown)$", doc, ignore.case = TRUE))
    stop("The current active document must be an R Markdown document. ",
      "I only see \"", basename(doc), "\".")
  d <- tfse::readlines(doc)
  et <- d[1:grep("^---", d)[2]]
  d <- d[-c(1:grep("^---", d)[2])]
  while (grepl("^$", d[1])) {
  	d <- d[-1]
  }
  x <- paste(d, collapse = "\n")
  x <- strsplit(x, "```")[[1]]
  chs <- grep("^\\{", x)
  x[chs] <- paste0("\n```", x[chs], "```\n")
  txt <- character()
  for (i in seq_along(chs)) {
  	st <- if (i == 1)
  		0 else
  			chs[i - 1]

  	txt[length(txt) + 1] <- paste0(c(reflow(
  		x[(1:length(x) > st) & (1:length(x) < chs[i])]), x[chs[i]]),
  		collapse = "")
  }
  if (length(x) > chs[i]) {
	  txt[length(txt) + 1] <- reflow(
	  	x[(1:length(x) > chs[i])])
  }
  x <- paste(c(et, txt), collapse = "\n")
  cat(x, file = doc, fill = TRUE)
}


reflow <- function(x) {
	x <- gsub("^\n+|\n+$", "", x)
	x[x == ""] <- "\n"
	x <- paste(x, collapse = "\n")
	x <- lapply(strsplit(x, "\n\n+")[[1]], stringr::str_wrap)
	paste0(c("", x, ""), collapse = "\n\n")
}

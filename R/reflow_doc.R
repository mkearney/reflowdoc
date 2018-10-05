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
  d
  y <- grep("^(\\s{0,}[1aA]{1}\\.|\\s{0,}[+-]{1}) ", d)
  yy <- c(y[-1], y[length(y)] + 1)
  st <- c(1, which((yy - y) > 1) + 1)
  st <- y[st]
  en <- c(which((yy - y) > 1), length(y))
  en <- y[en]
  s <- Map(seq, st, en)
  oy <- integer()
  items <- character()
  for (i in s) {
  	ii <- unlist(i)
  	items[length(items) + 1] <- paste(d[ii], collapse = "\n")
  	d[ii] <- ""
  	d[ii[1]] <- "ITEM_ITEM"
  }
  x <- paste(d, collapse = "\n")
  x <- gsub("\n{3,}", "\n\n", x)
  x <- strsplit(x, "```")[[1]]
  chs <- grep("^\\{", x)
  x[-chs] <- sub("^\n{2,}", "\n", x[-chs])
  x[chs] <- paste0("\n```", x[chs], "```\n")
  txt <- character()
  for (i in seq_along(chs)) {
  	st <- if (i == 1) {
  		0
  	} else {
  		chs[i - 1]
  	}
  	txt[length(txt) + 1] <- paste0(c(reflow(
  		x[(1:length(x) > st) & (1:length(x) < chs[i])]), x[chs[i]]),
  		collapse = "")
  }
  if (length(x) > chs[i]) {
	  txt[length(txt) + 1] <- reflow(
	  	x[(1:length(x) > chs[i])])
  }
  x <- paste(c(et, txt), collapse = "\n")
  x <- gsub("\n{3,}```", "\n\n```", x)
  x <- gsub("```\n{3,}", "```\n\n", x)
  for (i in seq_along(items)) {
  	ii <- items[[i]]
  	ii <- strsplit(ii, "\n")[[1]]
  	sp <- regexpr("^\\s+", ii)
  	if (any(sp > 0)) {
	  	spp <- which(sp > 0)
  		sps <- regmatches(ii, sp)
  	} else {
  		sps <- character()
  	}
  	ii <- unlist(lapply(ii, stringr::str_wrap, exdent = 2))
  	if (length(sps) > 0) {
  		ii[spp] <- paste0(sps, ii[spp])
  	}
  	ii <- paste(ii, collapse = "\n")
  	x <- sub("ITEM_ITEM", ii, x)
  }
  cat(x, file = doc, fill = TRUE)
}


reflow <- function(x) {
	#x <- gsub("^\n+|\n+$", "", x)
	x[x == ""] <- "\n"
	x <- paste(x, collapse = "\n")
	x <- gsub("\n{3,}", "\n\n", x)
	x <- lapply(strsplit(x, "\n\n+")[[1]], stringr::str_wrap)
	paste0("\n", paste0(x, collapse = "\n\n"), "\n")
}

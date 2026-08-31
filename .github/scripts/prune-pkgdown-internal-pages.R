args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args)) args[[1]] else "docs"

internal_pages <- c("AGENTS", "AI_CONTEXT", "copilot-instructions")
internal_html <- paste0(internal_pages, ".html")

unlink(
  file.path(site_dir, c(internal_html, paste0(internal_pages, ".md"))),
  force = TRUE
)

search_file <- file.path(site_dir, "search.json")
search_index <- jsonlite::read_json(search_file, simplifyVector = FALSE)
is_internal_search_entry <- function(entry) {
  path <- entry$path
  is.character(path) &&
    length(path) == 1L &&
    basename(sub("[?#].*$", "", path)) %in% internal_html
}
search_index <- Filter(Negate(is_internal_search_entry), search_index)
jsonlite::write_json(search_index, search_file, auto_unbox = TRUE)

sitemap_file <- file.path(site_dir, "sitemap.xml")
sitemap <- readLines(sitemap_file, warn = FALSE)
is_internal_sitemap_entry <- vapply(
  internal_html,
  function(page) grepl(paste0("/", page, "</loc>"), sitemap, fixed = TRUE),
  logical(length(sitemap))
)
sitemap <- sitemap[!rowSums(is_internal_sitemap_entry)]
writeLines(sitemap, sitemap_file)

remaining_files <- file.exists(file.path(site_dir, internal_html))
remaining_search_entries <- vapply(
  search_index,
  is_internal_search_entry,
  logical(1)
)
remaining_sitemap_entries <- vapply(
  internal_html,
  function(page) any(grepl(paste0("/", page, "</loc>"), sitemap, fixed = TRUE)),
  logical(1)
)

stopifnot(
  !any(remaining_files),
  !any(remaining_search_entries),
  !any(remaining_sitemap_entries)
)

message("Removed internal maintainer pages from ", site_dir)
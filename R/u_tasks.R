to_ascii_lower <- function(x) {
  x <- stringi::stri_trans_general(x, id = "Any-Latin;Greek-Latin;Latin-ASCII")
  tolower(x)
}

format_name <- function(x) {
  x <- stringi::stri_trans_general(x, id = "Any-Latin;Greek-Latin;Latin-ASCII")
  x <- tools::toTitleCase(x)
  gsub(pattern = "\\s+", replacement = "-", x)
}

decode_speciality <- function(code) {
  switch(
    code,
    "biochem" = "biochemija"           ,
    "biofiz"  = "biofizika"            ,
    "biolog"  = "biologija"            ,
    "genet"   = "genetika"             ,
    "mikrob"  = "mikrobiologija"       ,
    "molbio"  = "molekuline biologija" ,
    "neurbf"  = "neurobiofizika"       ,
    # stop("Unknown code = '", code, "'")
    "(?) UNIDENTIFIED"
  )
}

encode_speciality <- function(specialybe) {
  spec <- to_ascii_lower(specialybe)
  switch(
    spec,
    "biochemija"            = "biochem",
    "biofizika"             = "biofiz",
    "biologija"             = "biolog",
    "genetika"              = "genet",
    "mikrobiologija"        = "mikrob",
    "molekuline" = ,
    "molekuline biologija"  = "molbio",
    "neurobiofizika"        = "neurbf",
    stop("Unknown value: specialybe = '", specialybe, "'")
  )
}

#' Create Filename for Type U Task
#'
#' @name u_task
#'
#' @param uzduoties_nr (integer) The number of U task.
#' @param varianto_nr (integer) The variant of the task.
#'        Each student has a unique personal variant number.
#' @param specialybe (character) Name of study programme.
#' @param pavarde (character) Family name (names).
#' @param vardas (character) Sure name (names).
#' @param patikslinimas (character) Additional information.
#' @param dokumento_formatas (character) File name extension.
#'
#' @param x (character) A file name to parse or to check if it's structure
#'       is correct.
#'
#' @return
#' - `u_create_filename()` returns sting with formatted file name.
#' - `u_parse_filename()` returns data frame with details extracted from
#'                        the filename.
#' - `u_check_filename()` returns `TRUE` if the structure of file name is
#'                        correct and `FALSE` otherwise.
#'
#' @export
#' @examples
#' u_create_filename(
#'   uzduoties_nr       = 1,
#'   varianto_nr        = 000,
#'   specialybe         = "molekuline biologija",
#'   pavarde            = "Pavarde",
#'   vardas             = "Vardas Antras",
#'   patikslinimas      = "sertifikatas",
#'   dokumento_formatas = "pdf"
#' )
#'
#' u_check_filename("U01-v000-[molbio]-[Pavarde]-[Vardas-Antras]-sertifikatas.pdf")
#' u_parse_filename("U01-v000-[molbio]-[Pavarde]-[Vardas-Antras]-sertifikatas.pdf")
#'
#' u_parse_filename("U03-v000-[molekuline]-[Pavarde]-[Vardas].zip")
#'
#' # Incorrect family name:
#' u_parse_filename("U03-v000-[molekuline]-[Pavarde3]-[Vardas].zip")
u_create_filename <- function(
  uzduoties_nr,
  varianto_nr        = NULL,
  specialybe         = NULL,
  pavarde            = NULL,
  vardas             = NULL,
  patikslinimas    = "",
  dokumento_formatas = NULL
) {

  checkmate::assert_int(uzduoties_nr)
  checkmate::assert_int(varianto_nr)
  checkmate::assert_string(specialybe)
  checkmate::assert_string(pavarde)
  checkmate::assert_string(vardas)
  checkmate::assert_string(patikslinimas)
  checkmate::assert_string(dokumento_formatas)

  task <- sprintf("%02i", uzduoties_nr)
  vers <- sprintf("%03i", varianto_nr)

  pavarde <- format_name(pavarde)
  vard    <- format_name(vardas)
  more    <- to_ascii_lower(patikslinimas)
  more    <- if (more == "") "" else paste0("-", more)
  ext     <- to_ascii_lower(dokumento_formatas)
  spec    <- encode_speciality(specialybe)

  stringr::str_glue("U{task}-v{vers}-[{spec}]-[{pavarde}]-[{vard}]{more}.{ext}")
}

#' @rdname u_task
#' @export
u_parse_filename <- function(x) {
  rematch2::re_match(
    x,
    paste0(
      "(?<uzduoties_nr>U\\d{2})-v(?<varianto_nr>\\d{3})",
      "-\\[(?<specialybe>[a-z]*?)\\]",
      "-\\[(?<pavarde>[A-Za-z-]*?)\\]",
      "-\\[(?<vardas>[A-Za-z-]*?)\\]",
      "(-(?<patikslinimas>[a-z]*?))?",
      "[.](?<dokumento_formatas>[a-z]*?)$"
    )) %>%
    {purrr::quietly(tibble::as_tibble)}(.name_repair = "unique") %>%
    .$result %>%
    dplyr::select(-...6, -.text, -.match) %>%
    dplyr::mutate(
      specialybe = decode_speciality(specialybe),
      pavarde = stringr::str_replace_all(pavarde, "-", " "),
      vardas  = stringr::str_replace_all(vardas, "-", " "),
    ) %>%
    as.matrix() %>%
    t() %>%
    `colnames<-`("Reiksme") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Kategorija") %>%
    dplyr::mutate(Kategorija = paste0(Kategorija, ": "))
}

#' @rdname u_task
#' @export
u_check_filename <- function(x) {
  stringr::str_detect(
    x,
    paste0(
      "U(\\d{2})-v(\\d{3})", # technical info
      "-\\[[a-z]*?\\]-\\[([A-Za-z-]*?)\\]-\\[([A-Za-z-]*?)\\]", # user name info
      "(-[a-z]*?)?[.][a-z]*?" # additional info and extension
    ))
}

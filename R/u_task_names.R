#' Transliterate to ASCII Lower-Case Symbol
#'
#' Transliterate all symbols in a string into lower-case ASCII symbols.
#'
#'
#' @param x A string or a character vector.
#'
#' @return A modified string or character vector.
#'
#' @export
#'
#' @examples
#' to_ascii_lower("AbD")
#' to_ascii_lower(c("AAA", "BbB"))
#'
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
  dplyr::case_when(
    code %in% c("bioche", "biochemija")        ~ "biochemija"           ,
    code %in% c("biofiz", "biofizika")         ~ "biofizika"            ,
    code %in% c("biolog", "biologija")         ~ "biologija"            ,
    code %in% c("geneti", "genet", "genetika") ~ "genetika"             ,
    code %in% c("mikrob", "mikrobiologija")    ~ "mikrobiologija"       ,
    code %in% c("molbio", "molekuline")        ~ "molekuline biologija" ,
    code %in% c("molbti")                      ~ "molekuline biotechnologija",
    code %in% c("neurbf", "neurobiofizika")    ~ "neurobiofizika"       ,
    code %in% c("neurbl", "neurobiologija")    ~ "neurobiologija"       ,
    # stop("Unknown code = '", code, "'")
    TRUE ~ paste0("(?) UNIDENTIFIED VALUE { ", code ," }")
  )
}

encode_speciality <- function(specialybe) {
  spec <- to_ascii_lower(specialybe)

  supported <- c("biochemija", "biofizika", "biologija", "genetika",
    "mikrobiologija", "molekuline biologija", "molekuline biotechnologija",
    "neurobiofizika", "neurobiologija")

  not_supported <- specialybe[!spec %in% supported]
  if (length(not_supported) > 0) {
    stop("Unsupported value of 'specialybe': ", paste(not_supported, sep = ","))
  }

  dplyr::case_when(
    specialybe == "biochemija"                 ~ "bioche",
    specialybe == "biofizika"                  ~ "biofiz",
    specialybe == "biologija"                  ~ "biolog",
    specialybe == "genetika"                   ~ "geneti",
    specialybe == "mikrobiologija"             ~ "mikrob",
    specialybe == "molekuline biologija"       ~ "molbio",
    specialybe == "molekuline biotechnologija" ~ "molbti",
    specialybe == "neurobiofizika"             ~ "neurbf",
    specialybe == "neurobiologija"             ~ "neurbl",
    TRUE ~ paste0("(?) UNKNOWN { ", specialybe ," }")
    # TRUE ~ stop("Unknown value: specialybe = '", specialybe, "'")
  )
}

#' Create Filename for Type U Task
#'
#' Create a name for a file or folder which should be submitted as a "U" task.
#'
#' @name u_task
#'
#' @param uzduoties_nr (integer) The number of U task.
#' @param varianto_nr (integer) The variant of the task.
#'        Each student has a unique personal variant number.
#' @param specialybe (character) Name of study program.
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
#' u_create_filename(
#'   uzduoties_nr       = 10,
#'   varianto_nr        = 000,
#'   specialybe         = "molekuline biologija",
#'   pavarde            = "Pavarde",
#'   vardas             = "Vardas Antras"
#' )
#'
#' x <- "U01-v000-[molbio]-[Pavarde]-[Vardas-Antras]-sertifikatas.pdf"
#'
#' u_check_filename("U01-v000-[molbio]-[Pavarde]-[Vardas-Antras]-sertifikatas.pdf")
#' u_parse_filename("U01-v000-[molbio]-[Pavarde]-[Vardas-Antras]-sertifikatas.pdf")
#'
#' u_check_filename("U03-v000-[molekuline]-[Pavarde]-[Vardas].zip")
#' u_parse_filename("U03-v000-[molekuline]-[Pavarde]-[Vardas].zip")
#'
#' u_check_filename("U03-v000-[biofiz]-[Pavarde]-[Vardas]")
#' u_parse_filename("U03-v000-[biofiz]-[Pavarde]-[Vardas]")
#'
#' # Incorrect family name:
#' u_parse_filename("U03-v000-[molekuline]-[Pavarde3]-[Vardas].zip")
u_create_filename <- function(uzduoties_nr,
                              varianto_nr,
                              specialybe,
                              pavarde,
                              vardas,
                              patikslinimas = "",
                              dokumento_formatas = NULL) {
  checkmate::assert_int(uzduoties_nr)
  checkmate::assert_int(varianto_nr)
  checkmate::assert_string(specialybe)
  checkmate::assert_string(pavarde)
  checkmate::assert_string(vardas)
  checkmate::assert_string(patikslinimas)
  checkmate::assert_string(dokumento_formatas, null.ok = TRUE)

  task <- sprintf("%02i", uzduoties_nr)
  vers <- sprintf("%03i", varianto_nr)

  pavarde <- format_name(pavarde)
  vard    <- format_name(vardas)
  more    <- to_ascii_lower(patikslinimas)
  more    <- if (more == "") "" else paste0("-", more)

  if (is.null(dokumento_formatas)) {
    ext <- ""
  } else {
    ext <- paste0(".", to_ascii_lower(dokumento_formatas))
  }

  spec <- encode_speciality(specialybe)

  stringr::str_glue("U{task}-v{vers}-[{spec}]-[{pavarde}]-[{vard}]{more}{ext}")
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
      "([.](?<dokumento_formatas>[A-Za-z]*?))?$"
    )
  ) %>%
    {
      purrr::quietly(tibble::as_tibble)
    }(.name_repair = "unique") %>%
    .$result %>%
    dplyr::select(-...6, -...8, -.text, -.match) %>%
    dplyr::mutate(
      specialybe = decode_speciality(specialybe),
      pavarde = stringr::str_replace_all(pavarde, "-", " "),
      vardas = stringr::str_replace_all(vardas, "-", " "),
      patikslinimas =
        dplyr::if_else(
          !tolower(patikslinimas) %in% c("", "sertifikatas", "konspektas"),
          paste0("(?) UNIDENTIFIED VALUE { ", patikslinimas, " }"),
          patikslinimas
        ),
      dokumento_formatas =
        dplyr::case_when(
          dokumento_formatas == "" ~ "{ FOLDER }",
          tolower(dokumento_formatas) %in% c("pdf", "zip") ~ dokumento_formatas,
          TRUE ~ paste0("(?) UNSUPPORTED EXTENSION { ", dokumento_formatas, " }")
        )
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
      "(-[a-z]*?)?([.][a-z]*?)?$" # additional info and extension
    )
  )
}




#' Prepare Files For Submission (DataCamp Task)
#'
#' The function creates a folder (e.g.,`U00-atsakymai-ikelimui`) and copies files
#' indicated in `sertifikatas` and `konspektas` to that folder.
#' The copied files are properly renamed.
#' If needed, the function opens the folder and assignment submission webpage.
#'
#' @param uzduoties_nr (integer)
#' @param varianto_nr (integer)
#' @param specialybe (string)
#' @param pavarde (string)
#' @param vardas (string)
#' @param sertifikatas (string) Path to PDF file
#'        (certificate issued by DataCamp).
#' @param konspektas (string) Path to PDF file.
#' @param open_dir (`TRUE`|`FALSE`) Open the directory with submission files.
#' @param emokymai_id (integer) Open the directory with submission files.
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'   u_prepare_assignment_dc(
#'     uzduoties_nr = 1,
#'     varianto_nr  = 000,
#'     specialybe   = "molekuline biologija",
#'     pavarde      = "Pavarde",
#'     vardas       = "Vardas Antras",
#'     sertifikatas = "pratybos/certificate.pdf",
#'     konspektas   = "pratybos/konspektas.pdf"
#'   )
#' }
u_prepare_assignment_dc <- function(uzduoties_nr, varianto_nr, specialybe,
                                    pavarde, vardas, sertifikatas, konspektas,
                                    open_dir = FALSE, emokymai_id = NULL) {

  # Check input
  arguments <-
    c(
      "uzduoties_nr",
      "varianto_nr",
      "specialybe",
      "pavarde",
      "vardas",
      "sertifikatas",
      "konspektas"
    )
  missing_args <-
    c(
      missing(uzduoties_nr),
      missing(varianto_nr),
      missing(specialybe),
      missing(pavarde),
      missing(vardas),
      missing(sertifikatas),
      missing(konspektas)
    )

  if (any(missing_args)) {
    stop(
      "The following arguments are missing: ",
      paste(arguments[missing_args], collapse = ", ")
    )
  }

  # Check files
  if (!fs::file_exists(sertifikatas)) {
    stop("The file ('sertifikatas') was not found: ", sertifikatas)
  }

  if (!fs::file_exists(konspektas)) {
    stop("The file ('konspektas') was not found: ", konspektas)
  }

  # TODO: Check, if files are (valid) PDF


  # Main code
  f_name_s <- bio::u_create_filename(
    uzduoties_nr = uzduoties_nr,
    varianto_nr = varianto_nr,
    specialybe = specialybe,
    pavarde = pavarde,
    vardas = vardas,
    patikslinimas = "sertifikatas",
    dokumento_formatas = "pdf"
  )

  f_name_k <- bio::u_create_filename(
    uzduoties_nr = uzduoties_nr,
    varianto_nr = varianto_nr,
    specialybe = specialybe,
    pavarde = pavarde,
    vardas = vardas,
    patikslinimas = "konspektas",
    dokumento_formatas = "pdf"
  )

  # Open assignment submision webpage
  if (!is.null(emokymai_id)) {
    emokymai_submit_assingment(id = emokymai_id)
  }

  # Create directory
  n_dir <- sprintf("U%02d-atsakymai-ikelimui", uzduoties_nr)
  fs::dir_create(n_dir)
  if (isTRUE(open_dir)) {
    browseURL(n_dir)
  }

  # Copy and rename files
  c(
    fs::file_copy(sertifikatas, fs::path(n_dir, f_name_s), overwrite = TRUE),
    fs::file_copy(konspektas, fs::path(n_dir, f_name_k), overwrite = TRUE)
  )
}



#' Open Resource on Emokymai
#'
#' @name emokymai
#' @param id Resource ID.
#'
#' @export
emokymai_browse_assingment <- function(id) {
  e_url <- paste0("https://emokymai.vu.lt/mod/assign/view.php?id=", id)
  browseURL(e_url)
}

#' @rdname emokymai
#' @export
emokymai_submit_assingment <- function(id) {
  e_url <- paste0(
    "https://emokymai.vu.lt/mod/assign/view.php?id=", id,
    "&action=editsubmission"
  )
  browseURL(e_url)
}


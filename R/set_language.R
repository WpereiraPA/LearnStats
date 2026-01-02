#' Set Language for LearnStats Package
#'
#' Sets the interface language for the LearnStats package.
#' Currently supports English ("en") and Portuguese ("pt").
#'
#' @param lang Language code: "auto", "en", or "pt"
#' @return Invisibly returns the language code
#' @export
#'
#' @examples
#' set_language("pt")
#' set_language("en")
set_language <- function(lang = c("auto", "en", "pt")) {
  lang <- match.arg(lang)

  if (lang == "auto") {
    sys_lang <- tolower(Sys.getenv("LANG"))
    lang <- ifelse(grepl("^pt", sys_lang), "pt", "en")
  }

  options(LearnStats.lang = lang)

  messages <- list(
    en = "Language set to English",
    pt = "Idioma definido para Portugu\u00eas"
  )

  message(messages[[lang]])
  invisible(lang)
}

#' Get Current Language
#'
#' Returns the current language setting for LearnStats.
#'
#' @return Language code ("en" or "pt")
#' @export
#'
#' @examples
#' get_language()
get_language <- function() {
  lang <- getOption("LearnStats.lang", default = "en")
  if (!lang %in% c("en", "pt")) lang <- "en"
  lang
}

# Internal translation helper
.tr <- function(en_text, pt_text) {
  lang <- get_language()
  if (lang == "pt") pt_text else en_text
}

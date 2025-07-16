#' @importFrom xfun md5
#' @importFrom qs2 qs_save
#' @importFrom lobstr mem_used
#' @importFrom mirai require_daemons mirai daemons_set
#' @importFrom glue glue
NULL

wild_lobstr <- parse(text = ".__.bgjob_lobster_memusing. <- lobstr::mem_used()")

sessioninfohook <- parse(text = ".__.bgjob_sessionInfo. <- capture.output(sessionInfo())")

error_message_env <- new.env()

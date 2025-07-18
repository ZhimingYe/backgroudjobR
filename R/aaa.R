#' @importFrom qs2 qs_save
#' @importFrom mirai require_daemons mirai daemons_set
#' @importFrom glue glue
#' @importFrom tools md5sum
NULL

wild_lobstr <- parse(text = ".__.bgjob_lobster_memusing. <- backgroudjobR:::Mem_used()")

sessioninfohook <- parse(text = ".__.bgjob_sessionInfo. <- capture.output(sessionInfo())")

error_message_env <- new.env()

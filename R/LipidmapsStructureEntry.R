#' Lipidmaps Structure entry class.
#'
#' Entry class for Lipidmaps Structure.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('lipidmaps.structure')
#'
#' # Get an entry
#' e <- conn$getEntry('LMFA00000001')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @export
LipidmapsStructureEntry <- R6::R6Class("LipidmapsStructureEntry",
inherit=biodb::BiodbCsvEntry,

public=list(

initialize=function(...) {

    super$initialize(na.strings=c('', '-', 'NA'), quotes='"', sep="\t", ...)
}
),

private=list(
isContentCorrect=function(content) {
    valid <- ! grepl("No records? found", content)
    return(valid)
},

parseFieldsStep2=function(parsed.content) {

    # Set synonyms 
    if (self$hasField('SYNONYMS')) {
        v <- strsplit(self$getFieldValue('SYNONYMS'), ';')[[1]]
        v <- sub('^ +', '', v, perl=TRUE)
        v <- sub(' +$', '', v, perl=TRUE)
        self$setFieldValue('SYNONYMS', v)
    }

    # Synonyms
    if ('SYNONYMS' %in% names(parsed.content)) {
        v <- parsed.content[['SYNONYMS']]
        if ( ! is.na(v)) {
            v <- strsplit(v, ' *; *')[[1]]
            self$appendFieldValue('name', v)
        }
    }
}
))

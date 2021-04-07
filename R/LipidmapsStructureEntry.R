#' Lipidmaps Structure entry class.
#'
#' Entry class for Lipidmaps Structure.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
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
#' @export LipidmapsStructureEntry
#' @exportClass LipidmapsStructureEntry
LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry",
    contains='BiodbCsvEntry',

methods=list(

initialize=function(...) {

    callSuper(na.strings=c('', '-'), ...)
},

.isContentCorrect=function(content) {
    return( ! grepl("No records? found", content))
},

.parseFieldsStep2=function(parsed.content) {

    # Set synonyms 
    if (.self$hasField('SYNONYMS')) {
        v <- strsplit(.self$getFieldValue('SYNONYMS'), ';')[[1]]
        v <- sub('^ +', '', v, perl=TRUE)
        v <- sub(' +$', '', v, perl=TRUE)
        .self$setFieldValue('SYNONYMS', v)
    }

    # Synonyms
    if ('SYNONYMS' %in% names(parsed.content)) {
        v <- parsed.content[['SYNONYMS']]
        if ( ! is.na(v)) {
            v <- strsplit(v, ' *; *')[[1]]
            .self$appendFieldValue('name', v)
        }
    }
}

))

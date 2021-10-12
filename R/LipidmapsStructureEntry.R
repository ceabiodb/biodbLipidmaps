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
#' @importFrom R6 R6Class
#' @export
LipidmapsStructureEntry <- R6::R6Class("LipidmapsStructureEntry",
inherit=biodb::BiodbCsvEntry,

public=list(

#' @description
#' New instance initializer. Entry classes must not be instantiated
#' directly. Instead, you must use the getEntry() method of the connector class.
#' @param ... All parameters are passed to super class' initializer.
#' @return Nothing.
initialize=function(...) {
    super$initialize(na.strings=c('', '-', 'NA'), quotes='"', sep="\t", ...)
}
),

private=list(

doCheckContent=function(content) {
    valid <- ! grepl("No records? found", content)
    return(valid)
}

,doParseFieldsStep2=function(parsed.content) {

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

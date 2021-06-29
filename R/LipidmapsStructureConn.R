#' Lipidmaps Structure connector class.
#'
#' Connector class for Lipidmaps Structure.
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
LipidmapsStructureConn <- R6::R6Class("LipidmapsStructureConn",
inherit=biodb::BiodbConn,

public=list(

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    u <- self$getPropValSlot('urls', 'base.url')
    fct <- function(x) BiodbUrl$new(url=u, params=list(LMID=x))$toString()
    return(vapply(id, fct, FUN.VALUE=''))
},

#' @description
#' Calls LMSDSearch web service. See
#' https://www.lipidmaps.org/data/structure/programmaticaccess.html for
#' details.
#' @param mode The search mode: 'ProcessStrSearch', 'ProcessTextSearch' or
#' 'ProcessTextOntologySearch'. Compulsory.
#' @param output.mode If set to 'File', will output a in format `output.type`,
#' otherwise will output HTML.
#' @param output.type The output format: 'TSV', 'CSV' or 'SDF'.
#' @param output.delimiter The delimiter for TSV or CSV formats: 'Tab', 'Comma',
#' 'Semicolon'.
#' @param output.quote If quotes are to be used: 'Yes' or 'No'.
#' @param output.column.header If header must be output: 'Yes' or 'No'.
#' @param lmid a Lipidmaps ID.
#' @param name The name to search for.
#' @param formula The chemical formula to search for.
#' @param search.type The search type: 'SubStructure' or 'ExactMatch'.
#' @param smiles.string A SMILES to search for.
#' @param exact.mass The mass to search for.
#' @param exact.mass.offset The tolerance on the mass search.
#' @param core.class An integer number from 1 to 8.
#' @param main.class An integer number. See Lipidmaps documentation.
#' @param sub.class An integer number. See Lipidmaps documentation.
#' @param retfmt Use to set the format of the returned value. 'plain' will
#' return the raw results from the server, as a character value. 'request' will
#' return the request that would have been sent, as a BiodbRequest object.
#' 'parsed' will return data frame. 'ids' will return a character vector
#' containing the IDs of the matching entries.
#' @return Depending on `retfmt`.
wsLmsdSearch=function(mode=NULL, output.mode=NULL, output.type=NULL,
    output.delimiter=NULL, output.quote=NULL, output.column.header=NULL,
    lmid=NULL, name=NULL, formula=NULL, search.type=NULL, smiles.string=NULL,
    exact.mass=NULL, exact.mass.offset=NULL, core.class=NULL, main.class=NULL,
    sub.class=NULL, retfmt=c('plain', 'request', 'parsed', 'ids')) {

    retfmt <- match.arg(retfmt)

    # Set parameters for IDs
    if (retfmt == 'ids') {
        output.mode <- 'File'
        output.type <- 'TSV'
    }

    # Check parameters
    if ( ! is.null(mode) &&
        ! mode %in% c('ProcessStrSearch', 'ProcessTextSearch',
                      'ProcessTextOntologySearch'))
        biodb::error0('Unknown value "', output.mode,
                    '" for output.mode parameter.')
    if ( ! is.null(output.mode) && ! output.mode %in% c('File'))
        biodb::error0('Unknown value "', output.mode,
                    '" for output.mode parameter.')
    if ( ! is.null(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF'))
        biodb::error0('Unknown value "', output.type,
                    '" for output.type parameter.')
    if ( ! is.null(output.delimiter)
        && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
        biodb::error0('Unknown value "', output.delimiter,
                    '" for output.delimiter parameter.')
    if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
        biodb::error0('Unknown value "', output.quote,
                    '" for output.quote parameter.')
    if ( ! is.null(output.column.header)
        && ! output.column.header %in% c('Yes', 'No'))
        biodb::error0('Unknown value "', output.column.header,
                    '" for output.column.header parameter.')

    # Build request
    params <- list(Mode=mode)
    if ( ! is.null(output.mode))
        params <- c(params, OutputMode=output.mode)
    if ( ! is.null(output.type))
        params <- c(params, OutputType=output.type)
    if ( ! is.null(output.delimiter))
        params <- c(params, OutputDelimiter=output.delimiter)
    if ( ! is.null(output.quote))
        params <- c(params, OutputQuote=output.quote)
    if ( ! is.null(output.column.header))
        params <- c(params, OutputColumnHeader=output.column.header)
    if ( ! is.null(lmid))
        params <- c(params, LMID=lmid)
    if ( ! is.null(name))
        params <- c(params, Name=name)
    if ( ! is.null(formula))
        params <- c(params, Formula=formula)
    if ( ! is.null(search.type))
        params <- c(params, SearchType=search.type)
    if ( ! is.null(smiles.string))
        params <- c(params, SMILESString=smiles.string)
    if ( ! is.null(exact.mass))
        params <- c(params, ExactMass=exact.mass)
    if ( ! is.null(exact.mass.offset))
        params <- c(params, ExactMassOffSet=exact.mass.offset)
    if ( ! is.null(core.class))
        params <- c(params, CoreClass=core.class)
    if ( ! is.null(main.class))
        params <- c(params, MainClass=main.class)
    if ( ! is.null(sub.class))
        params <- c(params, SubClass=sub.class)
    u <- BiodbUrl$new(url=c(self$getPropValSlot('urls', 'base.url'),
                        'structure', 'LMSDSearch.php'), params=params)
    request <- self$makeRequest(method='get', url=u)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain' && output.mode == 'File') {

        # Mode must be set or HTML will be output
        if (is.null(output.type) || output.type %in% c('TSV', 'CSV')) {
            if (is.null(output.type) || output.type == 'TSV')
                sep <- "\t"
            else
                sep <- if (is.null(output.delimiter)
                           || output.delimiter == 'Comma') ',' else ';'
            header <- (is.null(output.column.header)
                       || output.column.header == 'Yes')
            quote <- if (is.null(output.quote)
                         || output.quote == 'No') '' else '"'
            results <- read.table(text=results, sep=sep, header=header,
                                  comment.char='', stringsAsFactors=FALSE,
                                  quote=quote, fill=TRUE)
        }
        else
            biodb::error('Only TSV and CSV output types are parsable.')

        # Extract IDs
        if (retfmt == 'ids')
            results <- results[['LM_ID']]
    }

    return(results)
},

#' @description
#' Calls LMSD web service for downloading one entry.
#' @return Depending on `retfmt`.
wsLmsd=function(lmid, format=c('tsv', 'csv'),
    retfmt=c('plain', 'request', 'parsed')) {

    format <- match.arg(format)
    retfmt <- match.arg(retfmt)
    chk::chk_string(lmid)

    # Build request
    url <- paste0(self$getPropValSlot('urls', 'lmsd.url'), lmid)
    params <- list(format=format)
    request <- self$makeRequest(method='get',
        url=BiodbUrl$new(url=url, params=params))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {
        sep <- if (format == 'tsv') "\t" else ','
        results <- read.table(text=results, sep=sep, header=TRUE,
                              comment.char='', stringsAsFactors=FALSE,
                              quote='"', fill=TRUE)
    }

    return(results)
},

#' @description
#' Calls LMSDRecord web service. See
#' http://www.lipidmaps.org/data/structure/programmaticaccess.html.
#' @param lmid A character vector containing the IDs of the wanted entries.
#' @param output.mode If set to 'File', will output a in format `output.type`,
#' otherwise will output HTML.
#' @param output.type The output format: 'TSV', 'CSV' or 'SDF'.
#' @param output.delimiter The delimiter for TSV or CSV formats: 'Tab', 'Comma',
#' 'Semicolon'.
#' @param output.quote If quotes are to be used: 'Yes' or 'No'.
#' @param output.column.header If header must be output: 'Yes' or 'No'.
#' @param retfmt Use to set the format of the returned value. 'plain' will
#' return the raw results from the server, as a character value. 'request' will
#' return the request that would have been sent, as a BiodbRequest object.
#' 'parsed' will return data frame.
#' @return Depending on `retfmt`.
wsLmsdRecord=function(lmid, mode=NULL, output.type=NULL, output.delimiter=NULL,
    output.quote=NULL, output.column.header=NULL,
    retfmt=c('plain', 'request', 'parsed')) {

    lifecycle::deprecate_stop('0.99.0', 'wsLmsdRecord()')

    retfmt <- match.arg(retfmt)
    chk::chk_string(lmid)

    # Check parameters
    if ( ! is.null(mode) && ! mode %in% c('File', 'Download'))
        biodb::error('Unknown value "%s" for mode parameter.', mode)
    if ( ! is.null(output.type)
        && ! output.type %in% c('TSV', 'CSV', 'SDF', 'MDLMOL'))
        biodb::error0('Unknown value "', output.type,
                    '" for output.type parameter.')
    if ( ! is.null(output.delimiter)
        && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
        biodb::error0('Unknown value "', output.delimiter,
                    '" for output.delimiter parameter.')
    if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
        biodb::error0('Unknown value "', output.quote,
                    '" for output.quote parameter.')
    if ( ! is.null(output.column.header)
        && ! output.column.header %in% c('Yes', 'No'))
        biodb::error0('Unknown value "', output.column.header,
                    '" for output.column.header parameter.')

    # Build request
    url <- paste0(self$getPropValSlot('urls', 'base.url'), 'LMSDRecord.php')
    params <- list(LMID=lmid)
    if ( ! is.null(mode))
        params <- c(params, Mode=mode)
    if ( ! is.null(output.type))
        params <- c(params, OutputType=output.type)
    if ( ! is.null(output.delimiter))
        params <- c(params, OutputDelimiter=output.delimiter)
    if ( ! is.null(output.quote))
        params <- c(params, OutputQuote=output.quote)
    if ( ! is.null(output.column.header))
        params <- c(params, OutputColumnHeader=output.column.header)
    request <- self$makeRequest(method='get',
                                 url=BiodbUrl$new(url=url, params=params))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain' && mode %in% c('File', 'Download')) {

        # Mode must be set or HTML will be output
        if ( ! is.null(output.type) && output.type %in% c('TSV', 'CSV')) {
            if (output.type == 'TSV')
                sep <- "\t"
            else
                sep <- if (is.null(output.delimiter)
                           || output.delimiter == 'Comma') ',' else ';'
            header <- (is.null(output.column.header)
                       || output.column.header == 'Yes')
            quote <- if (is.null(output.quote)
                         || output.quote == 'No') '' else '"'
            results <- read.table(text=results, sep=sep, header=header,
                                  comment.char='', stringsAsFactors=FALSE,
                                  quote=quote, fill=TRUE)
        }
        else
            biodb::error('Only TSV and CSV output types are parsable.')
    }

    return(results)
}
),

private=list(
doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- character()

    if ( ! is.null(fields)) {
        
        # Configure mass search
        exact.mass <- NULL
        exact.mass.offset <- NULL
        if ('monoisotopic.mass' %in% names(fields)) {
            rng <- do.call(Range$new, fields[['monoisotopic.mass']])
            exact.mass <- rng$getValue()
            exact.mass.offset <- rng$getDelta()
        }

        # Configure name search
        name <- fields$name

        # Search
        ids <- self$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                                  name=name, exact.mass=exact.mass,
                                  exact.mass.offset=exact.mass.offset,
                                  retfmt='ids')
    }

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

doGetEntryContentRequest=function(ids, concatenate=TRUE) {
    fct <- function(id) self$wsLmsd(lmid=id, format='tsv',
        retfmt='request')$getUrl()$toString()
    return(vapply(ids, fct, FUN.VALUE=''))
},

doGetEntryIds=function(max.results=NA_integer_) {

    # Retrieve all IDs
    ids <- self$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
        retfmt='ids')

    return(ids)
}
))

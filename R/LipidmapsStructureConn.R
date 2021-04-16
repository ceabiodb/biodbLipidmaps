#' Lipidmaps Structure connector class.
#'
#' Connector class for Lipidmaps Structure.
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
#' @import biodb
#' @import methods
#' @export LipidmapsStructureConn
#' @exportClass LipidmapsStructureConn
LipidmapsStructureConn <- methods::setRefClass("LipidmapsStructureConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),

methods=list(

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    u <- .self$getPropValSlot('urls', 'base.url')
    fct <- function(x) BiodbUrl(url=u, params=list(LMID=x))$toString()
    return(vapply(id, fct, FUN.VALUE=''))
},

wsLmsdSearch=function(mode=NULL, output.mode=NULL, output.type=NULL,
                      output.delimiter=NULL, output.quote=NULL,
                      output.column.header=NULL, lmid=NULL, name=NULL,
                      formula=NULL, search.type=NULL, smiles.string=NULL,
                      exact.mass=NULL, exact.mass.offset=NULL,
                      core.class=NULL, main.class=NULL, sub.class=NULL,
                      retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nCalls LMSDSearch web service. See
    https://www.lipidmaps.org/data/structure/programmaticaccess.html for
    details.
    \nmode: The search mode: 'ProcessStrSearch', 'ProcessTextSearch' or
    'ProcessTextOntologySearch'. Compulsory.
    \noutput.mode: If set to 'File', will output a in format `output.type`,
    otherwise will output HTML.
    \noutput.type: The output format: 'TSV', 'CSV' or 'SDF'.
    \noutput.delimiter: The delimiter for TSV or CSV formats: 'Tab', 'Comma',
    'Semicolon'.
    \noutput.quote: If quotes are to be used: 'Yes' or 'No'.
    \noutput.column.header: If header must be output: 'Yes' or 'No'.
    \nlmid: a Lipidmaps ID.
    \nname: The name to search for.
    \nformula: The chemical formula to search for.
    \nsearch.type: The search type: 'SubStructure' or 'ExactMatch'.
    \nsmiles.string: A SMILES to search for.
    \nexact.mass: The mass to search for.
    \nexact.mass.offset: The tolerance on the mass search.
    \ncore.class: An integer number from 1 to 8.
    \nmain.class: An integer number. See Lipidmaps documentation.
    \nsub.class: An integer number. See Lipidmaps documentation.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'request' will return
    the request that would have been sent, as a BiodbRequest object. 'parsed'
    will return data frame. 'ids' will return a character vector containing the
    IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

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
        .self$error('Unknown value "', output.mode,
                    '" for output.mode parameter.')
    if ( ! is.null(output.mode) && ! output.mode %in% c('File'))
        .self$error('Unknown value "', output.mode,
                    '" for output.mode parameter.')
    if ( ! is.null(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF'))
        .self$error('Unknown value "', output.type,
                    '" for output.type parameter.')
    if ( ! is.null(output.delimiter)
        && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
        .self$error('Unknown value "', output.delimiter,
                    '" for output.delimiter parameter.')
    if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
        .self$error('Unknown value "', output.quote,
                    '" for output.quote parameter.')
    if ( ! is.null(output.column.header)
        && ! output.column.header %in% c('Yes', 'No'))
        .self$error('Unknown value "', output.column.header,
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
    u <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'),
                        'structure', 'LMSDSearch.php'), params=params)
    request <- .self$makeRequest(method='get', url=u)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

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
            .self$error('Only TSV and CSV output types are parsable.')

        # Extract IDs
        if (retfmt == 'ids')
            results <- results[['LM_ID']]
    }

    return(results)
},

wsLmsdRecord=function(lmid, mode=NULL, output.type=NULL, output.delimiter=NULL,
                      output.quote=NULL, output.column.header=NULL,
                      retfmt=c('plain', 'request', 'parsed')) {
    ":\n\nCalls LMSDRecord web service. See
    http://www.lipidmaps.org/data/structure/programmaticaccess.html.
    \nlmid: A character vector containing the IDs of the wanted entries.
    \noutput.mode: If set to 'File', will output a in format `output.type`,
    otherwise will output HTML.
    \noutput.type: The output format: 'TSV', 'CSV' or 'SDF'.
    \noutput.delimiter: The delimiter for TSV or CSV formats: 'Tab', 'Comma',
    'Semicolon'.
    \noutput.quote: If quotes are to be used: 'Yes' or 'No'.
    \noutput.column.header: If header must be output: 'Yes' or 'No'.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'request' will return
    the request that would have been sent, as a BiodbRequest object. 'parsed'
    will return data frame.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Check parameters
    if ( ! is.null(mode) && ! mode %in% c('File', 'Download'))
        .self$error('Unknown value "', mode, '" for mode parameter.')
    if ( ! is.null(output.type)
        && ! output.type %in% c('TSV', 'CSV', 'SDF', 'MDLMOL'))
        .self$error('Unknown value "', output.type,
                    '" for output.type parameter.')
    if ( ! is.null(output.delimiter)
        && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
        .self$error('Unknown value "', output.delimiter,
                    '" for output.delimiter parameter.')
    if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
        .self$error('Unknown value "', output.quote,
                    '" for output.quote parameter.')
    if ( ! is.null(output.column.header)
        && ! output.column.header %in% c('Yes', 'No'))
        .self$error('Unknown value "', output.column.header,
                    '" for output.column.header parameter.')

    # Build request
    url <- paste0(.self$getPropValSlot('urls', 'base.url'), 'LMSDRecord.php')
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
    request <- .self$makeRequest(method='get', url=BiodbUrl(url=url, params=params))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

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
            .self$error('Only TSV and CSV output types are parsable.')
    }

    return(results)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {

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
        ids <- .self$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                                  name=name, exact.mass=exact.mass,
                                  exact.mass.offset=exact.mass.offset,
                                  retfmt='ids')
    }

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

.doGetEntryContentRequest=function(ids, concatenate=TRUE) {
    fct <- function(id) .self$wsLmsdRecord(lmid=id, mode='File',
                                           output.type='CSV',
                                           retfmt='request')$getUrl()$toString()
    return(vapply(ids, fct, FUN.VALUE=''))
},

.doGetEntryIds=function(max.results=NA_integer_) {

    # Retrieve all IDs
    ids <- .self$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                              retfmt='ids')

    return(ids)
}

))

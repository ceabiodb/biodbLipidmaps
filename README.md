<!-- vimvars: b:markdown_embedded_syntax={'r':''} -->
# biodbLipidmaps

An R  Bioconductor package for accessing [Lipidmaps](http://www.lipidmaps.org).
online database, based on Bioconductor package/framework
[biodb](https://github.com/pkrog/biodb/).

## Introduction

*biodbLipidmaps* is an extension package of the *biodb* package.
It allows to connect to Lipidmaps for retrieving entries, and searching for entries using the LMSDSearch web service.

## Examples

Getting a single entry:
```r
mybiodb <- biodb::newInst()
conn <- mybiodb$getFactory()$createConn('lipidmaps.structure')
entries <- conn$getEntry('LMFA00000001')
```

## Installation

Install the latest stable version using Bioconductor:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('biodbLipidmaps')
```

## Documentation

See the introduction vignette:
```r
vignette('intro', package='biodbLipidmaps')
```

## Citations

 * Sud M., Fahy E., Cotter D., Brown A., Dennis E., Glass C., Murphy R., Raetz C., Russell D., and Subramaniam S. LMSD: LIPID MAPS structure database. Nucleic Acids Research 35, D527-32 (2006), <https://doi.org/10.1093/nar/gkl838>.


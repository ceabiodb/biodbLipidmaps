# Makefile for biodb extensions packages, version 1.0

NEW_PKG=true

# Bioconductor check flags
BIOC_CHECK_FLAGS=quit-with-status no-check-formatting
ifeq (true,$(NEW_PKG))
BIOC_CHECK_FLAGS+=new-package
endif

# Set and check name
PKG_NAME := $(notdir $(realpath $(CURDIR)))
ifeq (,$(shell echo $(PKG_NAME) | grep '^biodb[A-Z]'))
$(error "$(PKG_NAME)" is not a standard package name for a biodb extension. The package name for a biodb extension must respect the format "^biodb[A-Z][A-Za-z0-9]*")
endif
PKG_NAME_CAPS := BIODB_$(shell echo $(PKG_NAME) | sed 's/^biodb//' | tr '[:lower:]' '[:upper:]')

# Check files
ifeq (,$(wildcard DESCRIPTION))
$(error Missing file DESCRIPTION)
endif

# Set cache folder
ifndef BIODB_CACHE_DIRECTORY
export BIODB_CACHE_DIRECTORY=$(CURDIR)/cache
endif

# Set testthat reporter
ifndef TESTTHAT_REPORTER
ifdef VIM
TESTTHAT_REPORTER=summary
else
TESTTHAT_REPORTER=progress
endif
endif

# Enable compiling
ifneq (,$(wildcard src))
COMPILE=compile
endif

# Get package version
PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')

# Set zip filename
ZIPPED_PKG=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Display values of main variables
$(info PKG_NAME=$(PKG_NAME))
$(info PKG_NAME_CAPS=$(PKG_NAME_CAPS))
$(info PKG_VERSION=$(PKG_VERSION))
$(info BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY))
$(info CODECOV_$(PKG_NAME_CAPS)_TOKEN=$(value CODECOV_$(PKG_NAME_CAPS)_TOKEN))

# Set R flags
RFLAGS=--slave --no-restore

# For R CMD SHLIB
export PKG_CXXFLAGS=$(shell R --slave -e "Rcpp:::CxxFlags()")

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif

# Default target
all: $(COMPILE)

# Compiling
ifdef COMPILE
compile: R/RcppExports.R
	R $(RFLAGS) CMD SHLIB -o src/$(PKG_NAME).so src/*.cpp

R/RcppExports.R: src/*.cpp
	R $(RFLAGS) -e "Rcpp::compileAttributes('$(CURDIR)')"
endif

# Code coverage
coverage:
	R $(RFLAGS) -e "covr::codecov(token='$(value CODECOV_$(PKG_NAME_CAPS)_TOKEN)', quiet=FALSE)"

# Bioconductor check
check: clean.vignettes $(ZIPPED_PKG)
	R $(RFLAGS) -e 'BiocCheck::BiocCheck("$(ZIPPED_PKG)"$(patsubst %,\, `%$`=TRUE,(BIOC_CHECK_FLAGS)))'

# Run testthat tests
test: $(COMPILE)
ifdef VIM
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))" | sed 's!\([^/A-Za-z_-]\)\(test[^/A-Za-z][^/]\+\.R\)!\1tests/testthat/\2!'
else
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))"
endif

# Launch Windows tests on server
win:
	R $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

# Build package zip
build: $(ZIPPED_PKG)

# Make zip
$(ZIPPED_PKG): doc
	R CMD build .

# Generation documentation
ifdef COMPILE
doc: R/RcppExports.R
else
doc:
endif
	R $(RFLAGS) -e "devtools::document('$(CURDIR)')"

# Generate vignettes
vignettes: clean.vignettes
	@echo Build vignettes for already installed package, not from local soures.
	R $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"

# Install dependencies
install.deps:
	R $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

# Install package
install: uninstall
	R $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies = TRUE)"

# Uninstall package
uninstall:
	R $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean all, included biodb cache
clean.all: clean clean.cache

# Clean
clean: clean.vignettes
ifdef COMPILE
	$(RM) src/*.o src/*.so src/*.dll
endif
	$(RM) -r tests/test.log tests/output tests/test\ *.log
	$(RM) -r $(PKG_NAME).Rcheck
	$(RM) -r Meta
	$(RM) $(PKG_NAME)_*.tar.gz

# Clean vignettes
clean.vignettes:
	$(RM) vignettes/*.R vignettes/*.html
	$(RM) -r doc

# Clean biodb cache
clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all win test build check vignettes install uninstall clean clean.all clean.vignettes clean.cache doc coverage

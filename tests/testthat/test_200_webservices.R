test_wsLmsdRecord_searchByName <- function(conn) {
    
	ids <- conn$wsLmsdSearch(mode='ProcessStrSearch', name='anhydride',
                             retfmt='ids')
    testthat::expect_vector(ids, character())
    testthat::expect_true(length(ids) >= 1)
    
	ids <- conn$wsLmsdSearch(mode='ProcessStrSearch', name='fatty',
                             retfmt='ids')
    testthat::expect_vector(ids, character())
    testthat::expect_true(length(ids) >= 5)
}

test_wsLmsdRecord <- function(conn) {

    # LMSDRecord.php is no more functional
    lifecycle::expect_defunct(conn$wsLmsdRecord(lmid='LMFA08040013', mode='File',
        output.type='CSV', retfmt='id'))

#	results <- conn$wsLmsdRecord(lmid='LMFA08040013')
#	expect_is(results, 'character')
#	expect_length(results, 1)
#
#	# No parsing possible (output.type not selected)
#	expect_error(conn$wsLmsdRecord(lmid='LMFA08040013', mode='File',
#                                   retfmt='parsed'),
#                 regexp='^.*Only TSV and CSV output types are parsable\\.$')
#
#	# Parse results successfully
#	results <- conn$wsLmsdRecord(lmid='LMFA08040013', mode='File',
#                                 output.type='CSV', retfmt='parsed')
#	expect_is(results, 'data.frame')
#	expect_equal(nrow(results), 1)
}

test_wsLmsdSearch <- function(conn) {

	results <- conn$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                               lmid='LMSL02000001')
	expect_is(results, 'character')
	expect_length(results, 1)

	results <- conn$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                              lmid='LMSL02000001', retfmt='parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)

	results <- conn$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                              lmid='LMSL02000001', retfmt='ids')
	expect_is(results, 'character')
	expect_length(results, 1)
	expect_equal(results, 'LMSL02000001')

	results <- conn$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                              name='acid', retfmt='parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)

	results <- conn$wsLmsdSearch(mode='ProcessStrSearch', output.mode='File',
                              name='acid', exact.mass=60.8,
                              exact.mass.offset=6, retfmt='parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)
}

# Set test context
biodb::testContext("Web services tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbLipidmaps')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('lipidmaps.structure')

biodb::testThat("wsLmsdRecord() can search by name.",
                test_wsLmsdRecord_searchByName, conn = conn)
biodb::testThat("Test web service wsLmsdRecord.",
                test_wsLmsdRecord, conn = conn)
biodb::testThat("Test web service wsLmsdSearch.",
                test_wsLmsdSearch, conn = conn)

# Terminate Biodb
biodb$terminate()

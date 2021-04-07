test.lipidmaps.structure.wsLmsdRecord <- function(conn) {

	results <- conn$wsLmsdRecord(lmid='LMFA08040013')
	expect_is(results, 'character')
	expect_length(results, 1)

	# No parsing possible (output.type not selected)
	expect_error(conn$wsLmsdRecord(lmid='LMFA08040013', mode='File',
                                   retfmt='parsed'),
                 regexp='^.*Only TSV and CSV output types are parsable\\.$')

	# Parse results successfully
	results <- conn$wsLmsdRecord(lmid='LMFA08040013', mode='File',
                                 output.type='CSV', retfmt='parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)
}

test.lipidmaps.structure.wsLmsdSearch <- function(conn) {

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
biodb <- biodb::createBiodbTestInstance(log='test_200_webservices.log',
                                        ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbLipidmaps')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('lipidmaps.structure')

biodb::testThat("Test web service wsLmsdRecord.",
                test.lipidmaps.structure.wsLmsdRecord, conn = conn)
biodb::testThat("Test web service wsLmsdSearch.",
                test.lipidmaps.structure.wsLmsdSearch, conn = conn)

# Terminate Biodb
biodb$terminate()

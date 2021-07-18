test_noOtherConnNeeded <- function(conn) {
    ids <- conn$getEntryIds(2)
    testthat::expect_is(ids, 'character')
    entries <- conn$getEntry(ids)
    testthat::expect_is(entries, 'list')
    x <- conn$getBiodb()$entriesToDataframe(entries)
    testthat::expect_is(x, 'data.frame')
}

# Set test context
biodb::testContext("Some other custom tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbLipidmaps')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('lipidmaps.structure')

biodb::testThat("No other connector is needed.", test_noOtherConnNeeded,
    conn=conn)

# Terminate Biodb
biodb$terminate()

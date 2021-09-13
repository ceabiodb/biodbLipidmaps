# Set test context
biodb::testContext("Generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbLipidmaps')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('lipidmaps.structure')

# Run generic tests
testRefFolder <- system.file("testref", package='biodbLipidmaps')
biodb::runGenericTests(conn, pkgName='biodbLipidmaps',
    testRefFolder=testRefFolder, opt=list(max.results=1))

# Terminate Biodb
biodb$terminate()

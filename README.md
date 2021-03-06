# forestMIDNarch
This package was originally called forestMIDN and was designed to connect with the original Access-based MIDN forest backend database. It is now being archived and forestMIDN is being updated to work with the new SQL-based backend. 

<h3>Package for importing, joining and querying MIDN forest database tables</h3>

The first step to using this package is to either execute the <b> importData() </b> or <b>importCSV()</b> function, which loads the most commonly used database tables into the global environment and names the tables accordingly. 

The <b>importData()</b> function uses an odbc driver for MSAccess to pull tables directly from the MIDN forest database. This requires an MSAccess odbc driver be installed on the machine, and the version of R (32 vs 64 bit) must match the version of Access. In the case of the MIDN forest database, this should be 32-bit R. This function can either use the master MIDN forest database named "MIDNFVM" as a SystemDSN, or a database path can be specified. This may only work on Windows-based operating systems, and requires odbc and DBI packages. 

The <b>importCSV()</b> function imports comma separated values files that were exported from the MIDN Forest Database. As long as the user has these exported .csv tables, this function will import and load all of the commonly used tables into the global environment and name the tables the same as the importData() function. 

The joinLocEvent(), joinTreeData(), joinRegenData(), joinQuadData(), and joinCWDData() source the tables from the global environment, and create common views of the data. These functions also allow the user to filter by common factors, such as park, years, exotic or native species, etc.

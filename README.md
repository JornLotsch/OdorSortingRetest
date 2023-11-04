# OdorSortingRetest
The unmodified code runs only on Unix like operating systems (Linux, MacOS) due to the uzsed type of parallel processing.
For Windows OS, line 67 must be changed to   
    SortingResults <- lapply(ListOfScoreTypes, function(ActualScoreType) {
and line 212 must be shortended to 
      })
or, the parallel computing must be implemented using the doParallel and foreach commands.

The implemetation of the 15 variants of code calculation can nevertheless be observed regardless of the OS. 


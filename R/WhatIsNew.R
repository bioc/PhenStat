WhatIsNew = function(){
cat('\n
What is new in Version', as.character(packageVersion("PhenStat")),':\n
  1. Statistical report generating function is added, see "PhenStatReport" \n
  2. Soft windowing function is added, see "PhenSmoothWin" \n
  3. Model weight is added, see "modelWeight" parameter in testDataset\n
  4. Global "plot()" function for PhenList and PhenTestResult objects is added\n
  5. Global "summary()" function for PhenTestResult object is added\n
  6. Bug fixed and minor improvements\n
    ')

}

require(devtools)
require(roxygen2)

roxygen2::roxygenize("./package")
#devtools::document("./package")
system("cmd /k \"R.exe CMD build ./package && R.exe CMD check *gz && exit\"")

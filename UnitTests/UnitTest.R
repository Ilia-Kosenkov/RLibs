# Load library for Unit Testing if it is not yet loaded/
if (!any(grepl("^package:testthat$", search())))
    library(testthat)

.RunTests <- function(reporter = SummaryReporter) {
    # If specific Test File is not selected, run for the whole directory
    if (!exists(".UnitTest_File")) {
        test_dir(file.path(getwd(), "UnitTests"), reporter = reporter)
    } else if (is.character(.UnitTest_File)) {
        # If variable is a non-null string, use it as test name
        if (nzchar(.UnitTest_File))
            test_file(file.path(getwd(), "UnitTests", .UnitTest_File),
            reporter = reporter)
        # Else, run for the whole directory
        else
            test_dir(file.path(getwd(), "UnitTests"), reporter = reporter)
    } else {
        # If variable is not a string, throw error.
        stop(sprintf("Unit test file name is not a valid string, but a '%s'.",
            paste(class(.UnitTest_File), collapse = "; ")))
}
}

.RunTests()

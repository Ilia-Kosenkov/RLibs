if (interactive()) {
    # If is in interactive session, run debug code

} else {

    message("Running `roxygen2::roxygenize`...")
    roxygen2::roxygenize("./package")

    pckgs <- base::dir(".", "*.gz")

    `%>%` <- dplyr::`%>%`
    stringr::str_match_all(pckgs, "RLibs_(([0-9]\\.){3}).*gz") %>%
        purrr::map(dplyr::as_tibble) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::select(1:2) %>%
        stats::setNames(nm = c("File", "Version")) %>%
        dplyr::arrange(desc(Version)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(File) -> latestPckg

    command <- sprintf("cmd /k \"R.exe CMD build ./package && R.exe CMD check %s && exit\"",
        latestPckg)
    message(paste("Executing:", command))
    system(command)

}
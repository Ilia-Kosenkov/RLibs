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


    isWin <- grepl("win(dows)?", Sys.info()["sysname"])
    if (is.na(isWin))
        stop("Unable to detect system. Run `R CMD build` manually.")

    sfx <- ifelse(isWin, ".exe", "")

    cmd_1 <- sprintf("R%s CMD build ./package", sfx)
    cmd_2 <- sprintf("R%s CMD check %s", sfx, latestPckg)
    #command <- sprintf("cmd /k \"R.exe CMD build ./package && R.exe CMD check %s && exit\"",
        #latestPckg)
    message(paste("Executing:", cmd_1))
    if(isWin)
        shell(cmd_1, mustWork = TRUE)
    else
        system(cmd_1)
    message(paste("Executing:", cmd_2))
    if(isWin)
        shell(cmd_2, mustWork = TRUE)
    else
        system(cmd_2)


}
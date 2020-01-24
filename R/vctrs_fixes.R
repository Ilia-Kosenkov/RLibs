# {vctrs} support for <glue> <--> <chracter> conversions
#' @title glue {vctrs} methods
#' @param x,y Vectors to check/cast.
#' @param to Target type.
#' @param x_arg,y_arg Names of the parameters to include in the error message.
#' @param ... Extra parameters.
#' @export
#' @rdname vctrs_glue
vec_ptype_abbr.glue <- function(x, ...) "glue"
#' @export
#' @rdname vctrs_glue
vec_ptype2.glue <- function(x, y, ...) UseMethod("vec_ptype2.glue", y)
#' @rdname vctrs_glue
#' @method vec_ptype2.glue default
#' @export
vec_ptype2.glue.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
#' @rdname vctrs_glue
#' @method vec_ptype2.glue fs_path
#' @export
vec_ptype2.glue.fs_path <- function(x, y, ..., x_arg = "x", y_arg = "y")
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
#' @rdname vctrs_glue
#' @method vec_ptype2.glue character
#' @export
vec_ptype2.glue.character <- function(x, y, ...) glue()
#' @rdname vctrs_glue
#' @method vec_ptype2.character glue
#' @export
vec_ptype2.character.glue <- function(x, y, ...) glue()
#' @rdname vctrs_glue
#' @method vec_ptype2.glue glue
#' @export
vec_ptype2.glue.glue <- function(x, y, ...) glue()

#' @rdname vctrs_glue
#' @export
vec_cast.glue <- function(x, to, ...) UseMethod("vec_cast.glue")
#' @rdname vctrs_glue
#' @method vec_cast.glue default
#' @export
vec_cast.glue.default <- function(x, to, ...)
    stop_incompatible_cast(x, to)
#' @rdname vctrs_glue
#' @method vec_cast.glue fs_path
#' @export
vec_cast.glue.fs_path <- function(x, to, ...)
    stop_incompatible_cast(x, to)
#' @rdname vctrs_glue
#' @method vec_cast.glue character
#' @export
vec_cast.glue.character <- function(x, to, ...) glue(x, .open = "", .close = "")
#' @rdname vctrs_glue
#' @method vec_cast.character glue
#' @export
vec_cast.character.glue <- function(x, to, ...) {
    out <- as.character(x)
    maybe_lossy_cast(out, x, to, lossy = TRUE)
}


# {vctrs} support for <fs_path> <--> <chracter> conversions
#' @title fs_path {vctrs} methods
#' @param x,y Vectors to check/cast.
#' @param to Target type.
#' @param ... Extra parameters.
#' @rdname vctrs_fs_path
#' @export
vec_ptype_abbr.fs_path <- function(x, ...) "fs_path"
#' @rdname vctrs_fs_path
#' @export
vec_ptype2.fs_path <- function(x, y, ...)
    UseMethod("vec_ptype2.fs_path", y)

#' @rdname vctrs_fs_path
#' @method vec_ptype2.fs_path glue
#' @export
vec_ptype2.fs_path.glue <- function(x, y, ...)
    stop_incompatible_type(x, y)

#' @rdname vctrs_fs_path
#' @method vec_ptype2.fs_path default
#' @export
vec_ptype2.fs_path.default <- function(x, y, ...)
    stop_incompatible_type(x, y)

#' @rdname vctrs_fs_path
#' @method vec_ptype2.fs_path character
#' @export
vec_ptype2.fs_path.character <- function(x, y, ...) fs::path()
#' @rdname vctrs_fs_path
#' @method vec_ptype2.character fs_path
#' @export
vec_ptype2.character.fs_path <- function(x, y, ...) fs::path()
#' @rdname vctrs_fs_path
#' @method vec_ptype2.fs_path fs_path
#' @export
vec_ptype2.fs_path.fs_path <- function(x, y, ...) fs::path()

#' @rdname vctrs_fs_path
#' @export
vec_cast.fs_path <- function(x, to, ...) UseMethod("vec_cast.fs_path", to)
#' @rdname vctrs_fs_path
#' @method vec_cast.fs_path default
#' @export
vec_cast.fs_path.default <- function(x, to, ...) #vec_default_cast(x, to)
    stop_incompatible_cast(x, to)
#' @rdname vctrs_fs_path
#' @method vec_cast.fs_path glue
#' @export
vec_cast.fs_path.glue <- function(x, to, ...) #vec_default_cast(x, to)
    stop_incompatible_cast(x, to)
#' @rdname vctrs_fs_path
#' @method vec_cast.fs_path character
#' @export
vec_cast.fs_path.character <- function(x, to, ...) fs::path(x)
#' @rdname vctrs_fs_path
#' @method vec_cast.character fs_path
#' @export
vec_cast.character.fs_path <- function(x, to, ...) {
    out <- as.character(x)
    maybe_lossy_cast(out, x, to, lossy = TRUE)
}
.onload <- function(libname,pkgname){
    # check whether env vars are set
    if ((Sys.getenv("R_DB_EJ") == "")){
        stop("must set the R_DB_EJ env variable")
    }
}

.onattach <- function(libname, pkgname){
    packageStartupMessage("Welcome to RESr!")
}

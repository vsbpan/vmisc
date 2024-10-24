# Helper function for wipping custom functions in the global env
wipe_functions <- function(){
  as.character(utils::lsf.str(pos = globalenv())) %>%
    lapply(function(x){
      rm(list = x, envir = globalenv())
    })
  invisible()
}


# Lazy wrapper for reloading fake package if it is already loaded
reload <- function(dev = FALSE, package = fake_pkg()){
  if(dev){
    source(paste(path.package(package), "init_dev.R", sep = "/"))
  } else {
    source(paste(path.package(package), "init.R", sep = "/"))
  }
}

# Recompile C++ code
recompile <- function(package = fake_pkg()){
  pkgload::load_all(path.package(package))
}


# Find fake package
fake_pkg <- function(fake_pkg_root_path = getwd()){
  pkg <- .packages()
  pkg[dirname(path.package(pkg)) %in% fake_pkg_root_path]
}


package_version2 <- function(x){
  tryCatch(utils::packageVersion(x),
           error = function(e){
             "version not found"
  })
}

.tidyverse_attach <- utils::getFromNamespace("tidyverse_attach", "tidyverse")
.confirm_conflict <- utils::getFromNamespace("confirm_conflict", "tidyverse")

.vmisc_dependencies <- function(include_self = TRUE){
  raw <- paste(utils::packageDescription("vmisc")$Imports,
               utils::packageDescription("vmisc")$Depends, sep = ",")
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
  if (include_self) {
    names <- c(names, "vmisc")
  }
  names
}

.vmisc_conflicts <- function(message = TRUE){
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- lapply(envs, function(env) {
    x <- ls(pos = env)
    x
  })
  if (length(objs) == 0) {
    objs <- objs
  }
  else {
    stacked <- utils::stack(objs)
    objs <- tapply(as.character(stacked$ind), stacked$values,
                   list)
  }
  conflicts <- purrr::keep(objs, ~length(.x) > 1)
  tidy_names <- paste0("package:", .vmisc_dependencies())
  conflicts <- purrr::keep(conflicts, ~any(.x %in% tidy_names))
  conflict_funs <- purrr::imap(conflicts, .confirm_conflict)
  x <- purrr::compact(conflict_funs)

  if (length(x) == 0)
    return("")
  header <- cli::rule(left = crayon::bold("Conflicts"), right = ".vmisc_conflicts()")
  pkgs <- x %>% purrr::map(~gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(others, names(others), ~paste0(crayon::blue(.x),
                                                                "::", .y, "()", collapse = ", "))
  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x),
                                                                         "()"))))
  bullets <- paste0(crayon::red(cli::symbol$cross), " ", funs,
                    " masks ", other_calls, collapse = "\n")
  res <- paste0(header, "\n", bullets)
  text_col(res)
}

detach.vmisc <- function(x){
  message("detatching package. . . ")
  if ("vmisc" %in% (.packages())) {
    detach("package:vmisc", unload = TRUE)
    message("goodbye!")
  }
  else {
    message("package is not loaded and therefore cannot be unloaded.")
  }
}

.reinstall.vmisc <- function(package_path = "C:/R_Projects/Package_Building/vmisc/vmisc_0.1.0.tar.gz", ...){
  load.vmisc <- "vmisc" %in% (.packages())
  detach.vmisc()
  utils::install.packages(package_path, repos = NULL, type = "source")
  rstudioapi::restartSession()
  if (load.vmisc) {
    library(vmisc)
  }
}

.onAttach <- function(...){
  packageStartupMessage(
    text_col(cli::rule(crayon::bold(paste0("Attached vmisc ", package_version2("vmisc")))))
  )
  packageStartupMessage("")
  .tidyverse_attach()
  if (!"package:conflicted" %in% search()) {
    packageStartupMessage(.vmisc_conflicts())
  }
}



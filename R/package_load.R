load_all2 <- function (path = ".", reset = TRUE, recompile = FALSE, export_all = TRUE,
                       helpers = TRUE, quiet = FALSE, ...) {
  if (inherits(path, "package")) {
    path <- path$path
  }
  .save_all()

  .load_all_inernal <- function (path = ".", reset = TRUE,
                                 compile = NA, attach = TRUE,
                                 export_all = TRUE, export_imports = export_all,
                                 helpers = export_all,
                                 attach_testthat = .uses_testthat(path),
                                 quiet = NULL, recompile = FALSE,
                                 warn_conflicts = TRUE) {
    path <- pkgload::pkg_path(path)
    package <- pkgload::pkg_name(path)
    description <- pkgload::pkg_desc(path)
    withr::local_envvar(c(DEVTOOLS_LOAD = package))
    quiet <- .load_all_quiet(quiet, "load_all")
    if (!quiet) {
      cli::cli_inform(c(i = "Loading {.pkg {package}}"))
    }
    if (package == "compiler") {
      oldEnabled <- compiler::enableJIT(0)
      on.exit(compiler::enableJIT(oldEnabled), TRUE)
    }
    if (missing(compile) && !missing(recompile)) {
      compile <- if (isTRUE(recompile))
        TRUE
      else NA
    }
    if (isTRUE(compile)) {
      pkgbuild::clean_dll(path)
      pkgbuild::compile_dll(path, quiet = quiet)
    }
    else if (identical(compile, NA)) {
      pkgbuild::compile_dll(path, quiet = quiet)
    }
    else if (identical(compile, FALSE)) {
    }
    else {
      cli::cli_abort("{.arg compile} must be a logical vector of length 1.")
    }
    old_methods <- list()
    if (reset) {
      .clear_cache()
      if (.is_loaded(package)) {
        .patch_colon(package)
        methods_env <- .ns_s3_methods(package)
        pkgload::unregister(package)
        old_methods <- as.list(methods_env)
        old_methods <- Filter(function(x) .is_foreign_method(x, package), old_methods)
      }
    }
    if (.is_loaded(package)) {
      rlang::env_unlock(pkgload::ns_env(package))
    }
    else {
      .create_ns_env(path)
    }
    out <- list(env = pkgload::ns_env(package))
    .load_depends(path, quiet = quiet)
    .load_imports(path)
    .insert_imports_shims(package)
    out$data <- pkgload::load_data(path)
    out$code <- pkgload::load_code(path, quiet = quiet)
    .register_s3(path)
    if (identical(compile, FALSE)) {
      out$dll <- .try_load_dll(path)
    }
    else {
      out$dll <- .load_dll2(path)
    }
    if (isTRUE(attach_testthat) && package != "testthat") {
      (`%:::%`("base", "library"))("testthat", warn.conflicts = FALSE)
    }
    .load_po(package, path)
    .run_pkg_hook(package, "load")
    .setup_ns_exports(path)
    .run_ns_load_actions(package)
    ns <- pkgload::ns_env(package)
    lockEnvironment(ns)
    for (nm in names(ns)) {
      lockBinding(nm, ns)
    }
    .run_user_hook(package, "load")
    if (attach) {
      .setup_pkg_env(package)
    }
    rlang::env_bind(.ns_s3_methods(package), !!!old_methods)
    if (attach) {
      .run_pkg_hook(package, "attach")
      .run_user_hook(package, "attach")
      .populate_pkg_env(package, path, export_all, export_imports,
                                 helpers)
    }
    .insert_global_shims()
    if (isTRUE(warn_conflicts)) {
      .warn_if_conflicts(package, out$env, globalenv())
    }
    invisible(out)
  }

  .load_dll2 <- function(path = "."){
    package <- pkgload::pkg_name(path)
    env <- pkgload::ns_env(package)
    nsInfo <- pkgload::parse_ns_file(path)
    dlls <- list()
    dynLibs <- nsInfo$dynlibs
    nativeRoutines <- list()
    for (i in seq_along(dynLibs)) {
      lib <- dynLibs[i]
      dlls[[lib]] <- .library.dynam3(path, lib)
      routines <- .assignNativeRoutines(dlls[[lib]], lib, env,
                                                 nsInfo$nativeRoutines[[lib]])
      nativeRoutines[[lib]] <- routines
      if (!is.null(names(nsInfo$dynlibs)) && nzchar(names(nsInfo$dynlibs)[i]))
        env[[names(nsInfo$dynlibs)[i]]] <- dlls[[lib]]
      setNamespaceInfo(env, "DLLs", dlls)
    }
    .addNamespaceDynLibs(env, nsInfo$dynlibs)
    dll_path <- dlls[[package]][["path"]]
    if (!rlang::is_null(dll_path)) {
      rlang::new_weakref(env, finalizer = .ns_finalizer(dll_path))
    }
    invisible(dlls)
  }

  .library.dynam3 <- function(path = ".", lib = ""){
    path <- pkgload::pkg_path(path)
    dyn_ext <- .Platform$dynlib.ext
    dllname <- paste(lib, dyn_ext, sep = "")
    dllfile <- pkgload::package_file("src", dllname, path = path)
    if (!file.exists(dllfile)) {
      return(invisible())
    }
    dllinfo <- dyn.load(dllfile)
    .dynLibs(c(.dynLibs(), list(dllinfo)))
    return(dllinfo)
  }

  .load_all_inernal(path = path, reset = reset, recompile = recompile,
                    export_all = export_all, helpers = helpers, quiet = quiet,
                    ...)
}

.save_all <- function(){
  if(rstudioapi::hasFun("documentSaveAll")) {
    rstudioapi::documentSaveAll()
  }
}


.insert_imports_shims <- utils::getFromNamespace("insert_imports_shims", "pkgload")
.addNamespaceDynLibs <- utils::getFromNamespace("addNamespaceDynLibs", "pkgload")
.is_foreign_method <- utils::getFromNamespace("is_foreign_method", "pkgload")
.patch_colon <- utils::getFromNamespace("patch_colon", "pkgload")
.ns_s3_methods <- utils::getFromNamespace("ns_s3_methods", "pkgload")
.is_loaded <- utils::getFromNamespace("is_loaded", "pkgload")
.clear_cache <- utils::getFromNamespace("clear_cache", "pkgload")
.load_all_quiet <- utils::getFromNamespace("load_all_quiet", "pkgload")
.uses_testthat <- utils::getFromNamespace("uses_testthat", "pkgload")
.load_depends <- utils::getFromNamespace("load_depends", "pkgload")
.load_imports <- utils::getFromNamespace("load_imports", "pkgload")
.run_pkg_hook <- utils::getFromNamespace("run_pkg_hook", "pkgload")
.load_po <- utils::getFromNamespace("load_po", "pkgload")
.run_user_hook <- utils::getFromNamespace("run_user_hook", "pkgload")
.setup_pkg_env <- utils::getFromNamespace("setup_pkg_env", "pkgload")
.populate_pkg_env <- utils::getFromNamespace("populate_pkg_env", "pkgload")
.insert_global_shims <- utils::getFromNamespace("insert_global_shims", "pkgload")
.assignNativeRoutines <- utils::getFromNamespace("assignNativeRoutines", "pkgload")
.addNamespaceDynLibs <- utils::getFromNamespace("addNamespaceDynLibs", "pkgload")
.ns_finalizer <- utils::getFromNamespace("ns_finalizer", "pkgload")
.warn_if_conflicts <- utils::getFromNamespace("warn_if_conflicts", "pkgload")
.create_ns_env <- utils::getFromNamespace("create_ns_env", "pkgload")
.register_s3 <- utils::getFromNamespace("register_s3", "pkgload")
.try_load_dll <- utils::getFromNamespace("try_load_dll", "pkgload")
.setup_ns_exports <- utils::getFromNamespace("setup_ns_exports", "pkgload")
.run_ns_load_actions <- utils::getFromNamespace("run_ns_load_actions", "pkgload")
.run_user_hook <- utils::getFromNamespace("run_user_hook", "pkgload")
`%:::%` <- utils::getFromNamespace("%:::%", "pkgload")




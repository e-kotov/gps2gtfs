# Note: Any variables prefixed with `.` are used for text
# replacement in the Makevars.in and Makevars.win.in

# Check if overrides are set in env vars
env_force_pure_r <- Sys.getenv("FORCE_PURE_R") != ""
env_force_no_rust <- Sys.getenv("FORCE_NO_RUST") != ""

# Check if a working C++ compiler is available
has_cxx_compiler <- if (env_force_pure_r) {
  FALSE
} else {
  tryCatch(
    {
      dummy_file <- tempfile(fileext = ".cpp")
      writeLines("int foo() { return 0; }", dummy_file)
      r_binary <- file.path(R.home("bin"), "R")
      shlib_cmd <- paste(shQuote(r_binary), "CMD SHLIB", shQuote(dummy_file))
      exit_code <- system(shlib_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
      dummy_base <- sub("\\.cpp$", "", dummy_file)
      exts <- c(".so", ".dll", ".dylib", ".o", ".obj")
      for (ext in exts) {
        f <- paste0(dummy_base, ext)
        if (file.exists(f)) invisible(file.remove(f))
      }
      if (file.exists(dummy_file)) {
        invisible(file.remove(dummy_file))
      }
      exit_code == 0
    },
    error = function(e) {
      FALSE
    }
  )
}

if (has_cxx_compiler) {
  if (env_force_no_rust) {
    message("Forcing Rust compilation skip.")
    has_rust <- FALSE
  } else {
    # check the packages MSRV first
    has_rust <- tryCatch(
      {
        source("tools/msrv.R")
        TRUE
      },
      error = function(e) {
        message(
          "Note: Rust toolchain (Cargo/rustc) not found or version is unsupported."
        )
        message("The package will be compiled WITHOUT Rust backend support.")
        FALSE
      }
    )
  }
} else {
  message("Note: Working C++ compiler not found.")
  message(
    "The package will be compiled as a PURE R package (WITHOUT Rcpp/Rust support)."
  )
  has_rust <- FALSE
}

# check DEBUG and NOT_CRAN environment variables
env_debug <- Sys.getenv("DEBUG")
env_not_cran <- Sys.getenv("NOT_CRAN")

# check if the vendored zip file exists
vendor_exists <- file.exists("src/rust/vendor.tar.xz")

is_not_cran <- env_not_cran != ""
is_debug <- env_debug != ""

if (is_debug) {
  # if we have DEBUG then we set not cran to true
  # CRAN is always release build
  is_not_cran <- TRUE
  message("Creating DEBUG build.")
}

if (!is_not_cran) {
  message("Building for CRAN.")
}

# we set cran flags only if NOT_CRAN is empty and if
# the vendored crates are present.
.cran_flags <- ifelse(
  !is_not_cran && vendor_exists,
  "-j 2 --offline",
  ""
)

# when DEBUG env var is present we use `--debug` build
.profile <- ifelse(is_debug, "", "--release")
.clean_targets <- ifelse(is_debug, "", "$(TARGET_DIR)")

# We specify this target when building for webR
webr_target <- "wasm32-unknown-emscripten"

# here we check if the platform we are building for is webr
is_wasm <- identical(R.version$platform, webr_target)

# print to terminal to inform we are building for webr
if (is_wasm) {
  message("Building for WebR")
}

# we check if we are making a debug build or not
# if so, the LIBDIR environment variable becomes:
# LIBDIR = $(TARGET_DIR)/{wasm32-unknown-emscripten}/debug
# this will be used to fill out the LIBDIR env var for Makevars.in
target_libpath <- if (is_wasm) "wasm32-unknown-emscripten" else NULL
cfg <- if (is_debug) "debug" else "release"

# used to replace @LIBDIR@
.libdir <- paste(c(target_libpath, cfg), collapse = "/")

# use this to replace @TARGET@
# we specify the target _only_ on webR
# there may be use cases later where this can be adapted or expanded
.target <- ifelse(is_wasm, paste0("--target=", webr_target), "")

# add panic exports only for WASM builds
.panic_exports <- ifelse(
  is_wasm,
  "CARGO_PROFILE_DEV_PANIC=\"abort\" CARGO_PROFILE_RELEASE_PANIC=\"abort\" ",
  ""
)

# read in the Makevars.in file checking
is_windows <- .Platform[["OS.type"]] == "windows"

# if windows we replace in the Makevars.win.in
mv_fp <- ifelse(
  is_windows,
  "src/Makevars.win.in",
  "src/Makevars.in"
)

# set the output file
mv_ofp <- ifelse(
  is_windows,
  "src/Makevars.win",
  "src/Makevars"
)

# delete the existing Makevars{.win/.wasm}
if (file.exists(mv_ofp)) {
  message("Cleaning previous `", mv_ofp, "`.")
  invisible(file.remove(mv_ofp))
}

# read as a single string
mv_txt <- readLines(mv_fp)

# define Rust-specific replacement strings
.statlib <- if (has_rust) "$(LIBDIR)/libgps2gtfs.a" else "rust_stub"
.pkg_libs <- if (has_rust) {
  if (is_windows) {
    "-L$(LIBDIR) -lgps2gtfs -lws2_32 -ladvapi32 -luserenv -lbcrypt -lntdll"
  } else {
    "-L$(LIBDIR) -lgps2gtfs"
  }
} else {
  ""
}
.pkg_cppflags <- if (has_rust) "-DHAS_RUST" else ""
.has_rust_str <- if (has_rust) "TRUE" else "FALSE"
.objects_decl <- if (has_cxx_compiler) "" else "OBJECTS = "

# replace placeholder values
new_txt <- gsub("@CRAN_FLAGS@", .cran_flags, mv_txt) |>
  gsub("@PROFILE@", .profile, x = _) |>
  gsub("@CLEAN_TARGET@", .clean_targets, x = _) |>
  gsub("@LIBDIR@", .libdir, x = _) |>
  gsub("@TARGET@", .target, x = _) |>
  gsub("@PANIC_EXPORTS@", .panic_exports, x = _) |>
  gsub("@STATLIB@", .statlib, x = _) |>
  gsub("@PKG_LIBS@", .pkg_libs, x = _) |>
  gsub("@PKG_CPPFLAGS@", .pkg_cppflags, x = _) |>
  gsub("@HAS_RUST@", .has_rust_str, x = _) |>
  gsub("@OBJECTS_DECL@", .objects_decl, x = _)

message("Writing `", mv_ofp, "`.")
con <- file(mv_ofp, open = "wb")
writeLines(new_txt, con, sep = "\n")
close(con)

# Conditionally update NAMESPACE
ns_fp <- "NAMESPACE"
if (file.exists(ns_fp)) {
  ns_txt <- readLines(ns_fp)
  if (!has_cxx_compiler) {
    message("Removing `useDynLib` from `NAMESPACE` for pure R installation.")
    ns_txt <- ns_txt[!grepl("useDynLib\\(", ns_txt)]
  } else {
    if (!any(grepl("useDynLib\\(gps2gtfs", ns_txt))) {
      ns_txt <- c(ns_txt, "useDynLib(gps2gtfs, .registration = TRUE)")
    }
  }
  writeLines(ns_txt, ns_fp)
}

message("`tools/config.R` has finished.")

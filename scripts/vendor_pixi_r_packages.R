# scripts/vendor_pixi_r_packages.R
#
# Pure-R packages from CRAN or R-universe:
#   1. Read repository metadata
#   2. Reject packages that need compilation/system requirements
#   3. Check Depends/Imports/LinkingTo against conda-forge
#   4. Download and unpack the source package into vendor/
#   5. Generate vendor/<Package>/pixi.toml for pixi-build-r
#   6. Add a path dependency to the parent pixi.toml
#
# Recommended invocation:
#   pixi run Rscript scripts/vendor_pixi_r_packages.R ExclusionTable forestploter

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

is_blank_field <- function(x) {
  length(x) == 0L || is.na(x) || !nzchar(trimws(x))
}

strip_ansi <- function(x) {
  gsub("\033\\[[0-9;?]*[ -/]*[@-~]", "", x, perl = TRUE)
}

r_package_to_conda_name <- function(package) {
  paste0("r-", tolower(package))
}

conda_safe_version <- function(version) {
  gsub("-", "_", version, fixed = TRUE)
}

base_and_recommended_r_packages <- function() {
  known <- c(
    # Base packages
    "base", "compiler", "datasets", "grDevices", "graphics", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "translations", "utils",
    # Recommended packages
    "KernSmooth", "MASS", "Matrix", "boot", "class", "cluster",
    "codetools", "foreign", "lattice", "mgcv", "nlme", "nnet",
    "rpart", "spatial", "survival"
  )

  installed <- tryCatch(
    rownames(
      installed.packages(
        priority = c("base", "recommended"),
        noCache = TRUE
      )
    ),
    error = function(e) character()
  )

  unique(c("R", known, installed))
}

parse_dependency_field <- function(value, source_package, dependency_type) {
  if (is_blank_field(value)) {
    return(data.frame(
      source_package = character(),
      dependency_type = character(),
      r_package = character(),
      constraint = character(),
      stringsAsFactors = FALSE
    ))
  }

  parts <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  parts <- parts[nzchar(parts)]

  names <- trimws(sub("\\s*\\(.*\\)\\s*$", "", parts))
  has_constraint <- grepl("\\(", parts, fixed = TRUE)
  constraints <- rep(NA_character_, length(parts))
  constraints[has_constraint] <- trimws(
    sub("^.*\\((.*)\\)\\s*$", "\\1", parts[has_constraint])
  )

  data.frame(
    source_package = source_package,
    dependency_type = dependency_type,
    r_package = names,
    constraint = constraints,
    stringsAsFactors = FALSE
  )
}

collect_strong_dependencies <- function(packages, package_db) {
  dependency_types <- c("Depends", "Imports", "LinkingTo")
  rows <- list()
  index <- 1L

  for (package in packages) {
    for (dependency_type in dependency_types) {
      parsed <- parse_dependency_field(
        value = package_db[package, dependency_type],
        source_package = package,
        dependency_type = dependency_type
      )

      if (nrow(parsed) > 0L) {
        rows[[index]] <- parsed
        index <- index + 1L
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      source_package = character(),
      dependency_type = character(),
      r_package = character(),
      constraint = character(),
      stringsAsFactors = FALSE
    ))
  }

  dependencies <- unique(do.call(rbind, rows))
  builtin <- base_and_recommended_r_packages()

  dependencies[
    !dependencies$r_package %in% builtin &
      nzchar(dependencies$r_package),
    ,
    drop = FALSE
  ]
}

read_parent_pixi_dependencies <- function(path = "pixi.toml") {
  if (!file.exists(path)) {
    return(character())
  }

  lines <- readLines(path, warn = FALSE)
  section_start <- grep("^\\s*\\[dependencies\\]\\s*$", lines)

  if (length(section_start) != 1L) {
    return(character())
  }

  section_start <- section_start[[1L]]
  all_sections <- grep("^\\s*\\[[^]]+\\]\\s*$", lines)
  later_sections <- all_sections[all_sections > section_start]
  section_end <- if (length(later_sections) == 0L) {
    length(lines) + 1L
  } else {
    later_sections[[1L]]
  }

  if (section_end <= section_start + 1L) {
    return(character())
  }

  block <- lines[(section_start + 1L):(section_end - 1L)]
  matches <- regexec("^\\s*([A-Za-z0-9_.-]+)\\s*=", block)
  values <- regmatches(block, matches)

  dependency_names <- vapply(
    values,
    function(x) if (length(x) >= 2L) x[[2L]] else NA_character_,
    character(1L)
  )

  unique(dependency_names[!is.na(dependency_names)])
}

pixi_search_package <- function(
  conda_package,
  channel = "https://prefix.dev/conda-forge",
  platform = "linux-64"
) {
  pixi <- Sys.which("pixi")

  if (!nzchar(pixi)) {
    stop(
      "`pixi` was not found in PATH. Run this script with `pixi run Rscript ...`.",
      call. = FALSE
    )
  }

  stdout_file <- tempfile("pixi-search-stdout-")
  stderr_file <- tempfile("pixi-search-stderr-")

  on.exit(
    unlink(c(stdout_file, stderr_file), force = TRUE),
    add = TRUE
  )

  status <- suppressWarnings(
    system2(
      command = pixi,
      args = c(
        "search",
        "--channel", channel,
        "--platform", platform,
        "--limit", "1",
        conda_package
      ),
      stdout = stdout_file,
      stderr = stderr_file,
      env = c("NO_COLOR=1")
    )
  )

  stdout <- if (file.exists(stdout_file)) {
    readLines(stdout_file, warn = FALSE)
  } else {
    character()
  }

  stderr <- if (file.exists(stderr_file)) {
    readLines(stderr_file, warn = FALSE)
  } else {
    character()
  }

  output <- strip_ansi(paste(c(stdout, stderr), collapse = "\n"))
  missing_pattern <- paste(
    c(
      "no package",
      "no packages",
      "not found",
      "no match",
      "no matches",
      "could not find",
      "package.*missing"
    ),
    collapse = "|"
  )

  if (identical(status, 0L) &&
      !grepl(missing_pattern, output, ignore.case = TRUE, perl = TRUE)) {
    return(list(
      state = "found",
      package = conda_package,
      output = output
    ))
  }

  if (grepl(missing_pattern, output, ignore.case = TRUE, perl = TRUE)) {
    return(list(
      state = "missing",
      package = conda_package,
      output = output
    ))
  }

  list(
    state = "error",
    package = conda_package,
    output = output,
    exit_status = status
  )
}

check_conda_forge_dependencies <- function(
  packages,
  package_db,
  parent_pixi = "pixi.toml",
  conda_channel = "https://prefix.dev/conda-forge",
  platform = "linux-64",
  verbose = TRUE
) {
  dependencies <- collect_strong_dependencies(packages, package_db)

  if (nrow(dependencies) == 0L) {
    message("No non-base Depends, Imports, or LinkingTo dependencies found.")
    return(invisible(dependencies))
  }

  dependencies$conda_package <- r_package_to_conda_name(
    dependencies$r_package
  )

  parent_dependencies <- read_parent_pixi_dependencies(parent_pixi)
  packages_being_vendored <- r_package_to_conda_name(packages)

  dependencies$provided_locally <- (
    dependencies$conda_package %in% parent_dependencies |
      dependencies$conda_package %in% packages_being_vendored
  )

  dependencies$conda_state <- NA_character_
  dependencies$conda_detail <- NA_character_

  search_cache <- new.env(parent = emptyenv())
  packages_to_search <- unique(
    dependencies$conda_package[!dependencies$provided_locally]
  )

  for (conda_package in packages_to_search) {
    if (verbose) {
      message("Checking conda-forge: ", conda_package)
    }

    result <- pixi_search_package(
      conda_package = conda_package,
      channel = conda_channel,
      platform = platform
    )

    assign(conda_package, result, envir = search_cache)
  }

  for (i in seq_len(nrow(dependencies))) {
    if (dependencies$provided_locally[[i]]) {
      dependencies$conda_state[[i]] <- "local"
      dependencies$conda_detail[[i]] <- (
        "already in parent pixi.toml or included in this vendor operation"
      )
    } else {
      result <- get(
        dependencies$conda_package[[i]],
        envir = search_cache,
        inherits = FALSE
      )

      dependencies$conda_state[[i]] <- result$state
      dependencies$conda_detail[[i]] <- result$output %||% ""
    }
  }

  search_errors <- dependencies[
    dependencies$conda_state == "error",
    ,
    drop = FALSE
  ]

  if (nrow(search_errors) > 0L) {
    details <- unique(vapply(
      seq_len(nrow(search_errors)),
      function(i) {
        sprintf(
          "  - %s\n    %s",
          search_errors$conda_package[[i]],
          gsub("\n", "\n    ", search_errors$conda_detail[[i]])
        )
      },
      character(1L)
    ))

    stop(
      paste(
        c(
          "pixi search failed while checking these dependencies:",
          details
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  missing <- dependencies[
    dependencies$conda_state == "missing",
    ,
    drop = FALSE
  ]

  if (nrow(missing) > 0L) {
    missing <- unique(
      missing[
        ,
        c(
          "source_package",
          "dependency_type",
          "r_package",
          "constraint",
          "conda_package"
        ),
        drop = FALSE
      ]
    )

    lines <- vapply(
      seq_len(nrow(missing)),
      function(i) {
        constraint <- if (is.na(missing$constraint[[i]])) {
          ""
        } else {
          paste0(" (", missing$constraint[[i]], ")")
        }

        sprintf(
          "  - %s%s -> %s [%s of %s]",
          missing$r_package[[i]],
          constraint,
          missing$conda_package[[i]],
          missing$dependency_type[[i]],
          missing$source_package[[i]]
        )
      },
      character(1L)
    )

    suggested_packages <- unique(missing$r_package)

    stop(
      paste(
        c(
          paste0(
            "The following direct strong dependencies were not found in ",
            "conda-forge for `", platform, "`:"
          ),
          "",
          lines,
          "",
          "Add these packages to the same vendor operation, for example:",
          paste0(
            "  vendor_pixi_r_packages(c(",
            paste(sprintf('"%s"', c(packages, suggested_packages)), collapse = ", "),
            "))"
          ),
          "",
          paste(
            "This check verifies package-name availability.",
            "Final version compatibility is still determined by `pixi install`."
          )
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  message(
    "Dependency check passed: all direct strong dependencies are ",
    "available on conda-forge or are managed locally."
  )

  invisible(dependencies)
}

validate_repository_metadata <- function(
  packages,
  package_db,
  strict_pure_r = TRUE
) {
  problems <- character()

  for (package in packages) {
    needs_compilation <- package_db[package, "NeedsCompilation"]
    system_requirements <- package_db[package, "SystemRequirements"]
    linking_to <- package_db[package, "LinkingTo"]

    if (strict_pure_r) {
      if (is.na(needs_compilation) ||
          tolower(trimws(needs_compilation)) != "no") {
        problems <- c(
          problems,
          sprintf(
            "%s: NeedsCompilation is `%s`, not `no`.",
            package,
            needs_compilation %||% "NA"
          )
        )
      }

      if (!is_blank_field(system_requirements)) {
        problems <- c(
          problems,
          sprintf(
            "%s: SystemRequirements is `%s`.",
            package,
            system_requirements
          )
        )
      }

      if (!is_blank_field(linking_to)) {
        problems <- c(
          problems,
          sprintf(
            "%s: LinkingTo is `%s`.",
            package,
            linking_to
          )
        )
      }
    }
  }

  if (length(problems) > 0L) {
    stop(
      paste(
        c(
          "These packages are outside the strict pure-R policy:",
          paste0("  - ", problems)
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

download_source_package <- function(
  package,
  package_db,
  repos,
  destination
) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  result <- utils::download.packages(
    pkgs = package,
    destdir = destination,
    available = package_db,
    repos = repos,
    type = "source",
    quiet = FALSE
  )

  if (is.null(result) || nrow(result) != 1L) {
    stop("Failed to download source package: ", package, call. = FALSE)
  }

  tarball <- result[1L, 2L]

  if (!file.exists(tarball)) {
    stop("Downloaded tarball was not found: ", tarball, call. = FALSE)
  }

  normalizePath(tarball)
}

extract_source_package <- function(
  tarball,
  expected_package,
  extraction_root
) {
  dir.create(extraction_root, recursive = TRUE, showWarnings = FALSE)
  utils::untar(tarball, exdir = extraction_root)

  top_level <- list.dirs(
    extraction_root,
    full.names = TRUE,
    recursive = FALSE
  )

  candidates <- top_level[
    file.exists(file.path(top_level, "DESCRIPTION"))
  ]

  if (file.exists(file.path(extraction_root, "DESCRIPTION"))) {
    candidates <- c(extraction_root, candidates)
  }

  if (length(candidates) == 0L) {
    stop(
      "No package root containing DESCRIPTION was found after extracting ",
      basename(tarball),
      ".",
      call. = FALSE
    )
  }

  candidate_names <- vapply(
    candidates,
    function(path) {
      description <- read.dcf(file.path(path, "DESCRIPTION"))
      description[1L, "Package"]
    },
    character(1L)
  )

  matched <- candidates[candidate_names == expected_package]

  if (length(matched) != 1L) {
    stop(
      "Could not uniquely identify package root for `",
      expected_package,
      "` after extraction.",
      call. = FALSE
    )
  }

  normalizePath(matched)
}

validate_extracted_source <- function(
  source_dir,
  strict_pure_r = TRUE
) {
  description_path <- file.path(source_dir, "DESCRIPTION")
  description <- read.dcf(description_path)

  package <- description[1L, "Package"]
  version <- description[1L, "Version"]
  needs_compilation <- if ("NeedsCompilation" %in% colnames(description)) {
    description[1L, "NeedsCompilation"]
  } else {
    NA_character_
  }
  system_requirements <- if ("SystemRequirements" %in% colnames(description)) {
    description[1L, "SystemRequirements"]
  } else {
    NA_character_
  }
  linking_to <- if ("LinkingTo" %in% colnames(description)) {
    description[1L, "LinkingTo"]
  } else {
    NA_character_
  }

  problems <- character()

  if (strict_pure_r) {
    if (is.na(needs_compilation) ||
        tolower(trimws(needs_compilation)) != "no") {
      problems <- c(
        problems,
        paste0("NeedsCompilation is `", needs_compilation %||% "NA", "`")
      )
    }

    if (!is_blank_field(system_requirements)) {
      problems <- c(
        problems,
        paste0("SystemRequirements is `", system_requirements, "`")
      )
    }

    if (!is_blank_field(linking_to)) {
      problems <- c(
        problems,
        paste0("LinkingTo is `", linking_to, "`")
      )
    }

    if (dir.exists(file.path(source_dir, "src"))) {
      problems <- c(problems, "a src/ directory exists")
    }

    if (file.exists(file.path(source_dir, "configure")) ||
        file.exists(file.path(source_dir, "configure.win"))) {
      problems <- c(problems, "a configure or configure.win script exists")
    }
  }

  if (length(problems) > 0L) {
    stop(
      paste0(
        package,
        " is outside the strict pure-R policy: ",
        paste(problems, collapse = "; "),
        "."
      ),
      call. = FALSE
    )
  }

  list(
    package = package,
    version = version,
    description = description
  )
}

write_child_pixi_toml <- function(
  source_dir,
  package,
  version,
  platform = "linux-64",
  r_base_spec = ">=4.5,<4.6",
  conda_channel = "https://prefix.dev/conda-forge",
  backend_channel = "https://prefix.dev/pixi-build-backends",
  backend_version = "*"
) {
  conda_name <- r_package_to_conda_name(package)

  lines <- c(
    "[workspace]",
    sprintf('channels = ["%s"]', conda_channel),
    sprintf('platforms = ["%s"]', platform),
    'preview = ["pixi-build"]',
    "",
    "[package]",
    sprintf('name = "%s"', conda_name),
    sprintf('version = "%s"', conda_safe_version(version)),
    "",
    "[package.build.backend]",
    'name = "pixi-build-r"',
    sprintf('version = "%s"', backend_version),
    "channels = [",
    sprintf('  "%s",', backend_channel),
    sprintf('  "%s",', conda_channel),
    "]",
    "",
    "[package.build.config]",
    "compilers = []",
    "",
    "[package.host-dependencies]",
    sprintf('r-base = "%s"', r_base_spec),
    ""
  )

  writeLines(lines, file.path(source_dir, "pixi.toml"))
  invisible(file.path(source_dir, "pixi.toml"))
}

copy_directory <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)

  files <- list.files(
    from,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  copied <- file.copy(
    from = files,
    to = to,
    recursive = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (length(copied) > 0L && !all(copied)) {
    stop("Failed to copy all files from ", from, " to ", to, ".", call. = FALSE)
  }

  invisible(TRUE)
}

update_parent_pixi_dependencies <- function(
  parent_pixi,
  dependency_lines,
  replace_existing = FALSE
) {
  if (!file.exists(parent_pixi)) {
    stop("Parent manifest was not found: ", parent_pixi, call. = FALSE)
  }

  lines <- readLines(parent_pixi, warn = FALSE)
  section_start <- grep("^\\s*\\[dependencies\\]\\s*$", lines)

  if (length(section_start) != 1L) {
    stop(
      "Expected exactly one top-level [dependencies] section in ",
      parent_pixi,
      ".",
      call. = FALSE
    )
  }

  section_start <- section_start[[1L]]
  all_sections <- grep("^\\s*\\[[^]]+\\]\\s*$", lines)
  later_sections <- all_sections[all_sections > section_start]
  section_end <- if (length(later_sections) == 0L) {
    length(lines) + 1L
  } else {
    later_sections[[1L]]
  }

  before <- lines[seq_len(section_start)]
  block <- if (section_end > section_start + 1L) {
    lines[(section_start + 1L):(section_end - 1L)]
  } else {
    character()
  }
  after <- if (section_end <= length(lines)) {
    lines[section_end:length(lines)]
  } else {
    character()
  }

  for (dependency_line in dependency_lines) {
    dependency_name <- sub("\\s*=.*$", "", dependency_line)
    block_keys <- vapply(
      block,
      function(line) {
        match <- regexec("^\\s*([A-Za-z0-9_.-]+)\\s*=", line)
        value <- regmatches(line, match)[[1L]]
        if (length(value) >= 2L) value[[2L]] else NA_character_
      },
      character(1L)
    )

    existing <- which(!is.na(block_keys) & block_keys == dependency_name)

    if (length(existing) > 1L) {
      stop(
        "Duplicate dependency entries found for `",
        dependency_name,
        "` in ",
        parent_pixi,
        ".",
        call. = FALSE
      )
    }

    if (length(existing) == 1L) {
      normalized_existing <- gsub("\\s+", "", block[existing])
      normalized_new <- gsub("\\s+", "", dependency_line)

      if (identical(normalized_existing, normalized_new)) {
        next
      }

      if (!replace_existing) {
        stop(
          "Dependency `",
          dependency_name,
          "` already exists in ",
          parent_pixi,
          ". Set `replace_existing = TRUE` to replace it.",
          call. = FALSE
        )
      }

      block[existing] <- dependency_line
    } else {
      block <- c(block, dependency_line)
    }
  }

  output <- c(before, block, after)
  temporary <- tempfile(
    pattern = paste0(basename(parent_pixi), "-"),
    tmpdir = dirname(parent_pixi)
  )

  writeLines(output, temporary)

  if (!file.rename(temporary, parent_pixi)) {
    unlink(temporary, force = TRUE)
    stop("Failed to update ", parent_pixi, ".", call. = FALSE)
  }

  invisible(TRUE)
}

vendor_pixi_r_packages <- function(
  packages,
  repos = c(CRAN = "https://cloud.r-project.org"),
  vendor_dir = "vendor",
  parent_pixi = "pixi.toml",
  platform = "linux-64",
  r_base_spec = ">=4.5,<4.6",
  conda_channel = "https://prefix.dev/conda-forge",
  backend_channel = "https://prefix.dev/pixi-build-backends",
  backend_version = "*",
  strict_pure_r = TRUE,
  check_dependencies = TRUE,
  update_parent = TRUE,
  overwrite_vendor = FALSE,
  replace_existing = FALSE,
  dry_run = FALSE,
  verbose = TRUE
) {
  packages <- unique(trimws(packages))
  packages <- packages[nzchar(packages)]

  if (length(packages) == 0L) {
    stop("Specify at least one R package.", call. = FALSE)
  }

  if (!file.exists(parent_pixi) && update_parent) {
    stop("Parent pixi.toml was not found: ", parent_pixi, call. = FALSE)
  }

  fields <- c("NeedsCompilation", "SystemRequirements")
  package_db <- utils::available.packages(
    repos = repos,
    type = "source",
    fields = fields,
    filters = c("R_version", "OS_type", "subarch", "duplicates")
  )

  missing_from_repositories <- setdiff(packages, rownames(package_db))

  if (length(missing_from_repositories) > 0L) {
    stop(
      paste0(
        "Packages not found in the configured repositories: ",
        paste(missing_from_repositories, collapse = ", "),
        "\nRepositories: ",
        paste(repos, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  package_db <- package_db[packages, , drop = FALSE]

  validate_repository_metadata(
    packages = packages,
    package_db = package_db,
    strict_pure_r = strict_pure_r
  )

  if (check_dependencies) {
    check_conda_forge_dependencies(
      packages = packages,
      package_db = package_db,
      parent_pixi = parent_pixi,
      conda_channel = conda_channel,
      platform = platform,
      verbose = verbose
    )
  }

  dir.create(vendor_dir, recursive = TRUE, showWarnings = FALSE)

  destinations <- file.path(vendor_dir, packages)
  already_exists <- destinations[dir.exists(destinations)]

  if (length(already_exists) > 0L && !overwrite_vendor) {
    stop(
      paste0(
        "These vendor directories already exist:\n  - ",
        paste(already_exists, collapse = "\n  - "),
        "\nSet `overwrite_vendor = TRUE` to replace them."
      ),
      call. = FALSE
    )
  }

  stage_root <- file.path(
    vendor_dir,
    paste0(
      ".pixi-r-stage-",
      Sys.getpid(),
      "-",
      format(Sys.time(), "%Y%m%d%H%M%S")
    )
  )
  download_dir <- file.path(stage_root, "downloads")
  extract_root <- file.path(stage_root, "extracted")

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(extract_root, recursive = TRUE, showWarnings = FALSE)

  on.exit(
    unlink(stage_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  staged <- vector("list", length(packages))
  names(staged) <- packages

  for (package in packages) {
    if (verbose) {
      message(
        "Preparing ",
        package,
        " ",
        package_db[package, "Version"],
        "..."
      )
    }

    tarball <- download_source_package(
      package = package,
      package_db = package_db,
      repos = repos,
      destination = download_dir
    )

    package_extract_root <- file.path(extract_root, package)
    source_dir <- extract_source_package(
      tarball = tarball,
      expected_package = package,
      extraction_root = package_extract_root
    )

    metadata <- validate_extracted_source(
      source_dir = source_dir,
      strict_pure_r = strict_pure_r
    )

    write_child_pixi_toml(
      source_dir = source_dir,
      package = metadata$package,
      version = metadata$version,
      platform = platform,
      r_base_spec = r_base_spec,
      conda_channel = conda_channel,
      backend_channel = backend_channel,
      backend_version = backend_version
    )

    staged[[package]] <- list(
      source_dir = source_dir,
      package = metadata$package,
      version = metadata$version,
      destination = file.path(vendor_dir, package),
      conda_name = r_package_to_conda_name(package)
    )
  }

  plan <- data.frame(
    package = packages,
    version = vapply(staged, `[[`, character(1L), "version"),
    conda_package = vapply(staged, `[[`, character(1L), "conda_name"),
    destination = vapply(staged, `[[`, character(1L), "destination"),
    stringsAsFactors = FALSE
  )

  if (dry_run) {
    message("Dry run completed. No vendor directories or pixi.toml were changed.")
    return(invisible(plan))
  }

  for (package in packages) {
    item <- staged[[package]]
    destination <- item$destination

    if (dir.exists(destination)) {
      unlink(destination, recursive = TRUE, force = TRUE)
    }

    moved <- file.rename(item$source_dir, destination)

    if (!moved) {
      copy_directory(item$source_dir, destination)
    }

    if (!file.exists(file.path(destination, "DESCRIPTION")) ||
        !file.exists(file.path(destination, "pixi.toml"))) {
      stop(
        "Vendor operation did not produce the expected files for ",
        package,
        ".",
        call. = FALSE
      )
    }

    message(
      "Vendored ",
      package,
      " ",
      item$version,
      " -> ",
      destination
    )
  }

  if (update_parent) {
    dependency_lines <- sprintf(
      '%s = { path = "%s" }',
      plan$conda_package,
      plan$destination
    )

    update_parent_pixi_dependencies(
      parent_pixi = parent_pixi,
      dependency_lines = dependency_lines,
      replace_existing = replace_existing
    )

    message("Updated ", parent_pixi, ".")
  }

  message("")
  message("Next step:")
  message("  pixi install 2>&1 | tee pixi-install.log")

  invisible(plan)
}


# Allow direct command-line use:
#
#   pixi run Rscript scripts/vendor_pixi_r_packages.R \
#     ExclusionTable forestploter
#
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0L) {
    stop(
      paste(
        "Usage:",
        "  pixi run Rscript scripts/vendor_pixi_r_packages.R PACKAGE [PACKAGE ...]"
      ),
      call. = FALSE
    )
  }

  vendor_pixi_r_packages(args)
}

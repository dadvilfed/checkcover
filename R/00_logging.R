#### GLOBAL LOGGING SYSTEM ####

.CHECKOVER_LOG <- new.env(parent = emptyenv())
.CHECKOVER_LOG$level_map <- c(DEBUG = 10L, INFO = 20L, WARN = 30L, ERROR = 40L)
.CHECKOVER_LOG$current_level <- "INFO"
.CHECKOVER_LOG$file_con <- NULL

.log_is_enabled <- function(level) {
  level <- toupper(level)
  lm <- .CHECKOVER_LOG$level_map
  lm[[level]] >= lm[[.CHECKOVER_LOG$current_level]]
}

.log_write <- function(level, msg, module = NULL) {
  level <- toupper(level)
  if (!.log_is_enabled(level)) return(invisible(NULL))
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  module_tag <- if (!is.null(module) && nzchar(module)) paste0("[", module, "] ") else ""
  line <- sprintf("%s [%s] %s%s", timestamp, level, module_tag, msg)
  
  cat(line, "\n")
  flush.console()
  
  con <- .CHECKOVER_LOG$file_con
  if (!is.null(con) && isOpen(con)) {
    writeLines(line, con)
    flush(con)
  }
  invisible(line)
}

# Public API
init_logger <- function(log_dir = "checkover_output/logs",
                        log_level = c("INFO", "DEBUG", "WARN", "ERROR"),
                        log_file = NULL, overwrite = FALSE) {
  log_level <- toupper(match.arg(log_level))
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  
  if (is.null(log_file)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(log_dir, paste0("checkover_", stamp, ".log"))
  }
  
  if (!is.null(.CHECKOVER_LOG$file_con)) {
    try(close(.CHECKOVER_LOG$file_con), silent = TRUE)
  }
  
  .CHECKOVER_LOG$file_con <- file(log_file, open = if(overwrite) "w" else "a")
  .CHECKOVER_LOG$current_level <- log_level
  
  .log_write("INFO", sprintf("Logger initialised. Log file: %s", log_file), module = "GLOBAL")
  invisible(log_file)
}

close_logger <- function() {
  if (!is.null(.CHECKOVER_LOG$file_con)) {
    .log_write("INFO", "Closing logger.", module = "GLOBAL")
    try(close(.CHECKOVER_LOG$file_con), silent = TRUE)
    .CHECKOVER_LOG$file_con <- NULL
  }
  invisible(NULL)
}

log_debug <- function(msg, ..., module = NULL) {
  .log_write("DEBUG", sprintf(msg, ...), module = module)
}

log_info <- function(msg, ..., module = NULL) {
  .log_write("INFO", sprintf(msg, ...), module = module)
}

log_warn <- function(msg, ..., module = NULL) {
  .log_write("WARN", sprintf(msg, ...), module = module)
}

log_error <- function(msg, ..., module = NULL) {
  .log_write("ERROR", sprintf(msg, ...), module = module)
}

# Memory logging
.mem_state <- new.env(parent = emptyenv())
.mem_state$peak_gc_mb  <- NA_real_
.mem_state$peak_rss_mb <- NA_real_

.mem_snapshot <- function() {
  gc_mat <- suppressWarnings(gc())
  gc_mb  <- NA_real_
  if (is.matrix(gc_mat) && "(Mb)" %in% colnames(gc_mat)) {
    gc_mb <- sum(gc_mat[, "(Mb)"], na.rm = TRUE)
  }
  
  rss_mb <- NA_real_
  if (.Platform$OS.type == "windows" && exists("memory.size")) {
    rss_mb <- suppressWarnings(as.numeric(memory.size()))
  } else if (file.exists("/proc/self/status")) {
    st <- try(readLines("/proc/self/status"), silent = TRUE)
    if (!inherits(st, "try-error")) {
      ln <- st[grep("^VmRSS:", st)][1]
      if (!is.na(ln)) {
        kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", ln)))
        if (is.finite(kb)) rss_mb <- kb / 1024
      }
    }
  }
  
  list(gc_total_mb = gc_mb, rss_mb = rss_mb)
}

log_memory <- function(label = NULL, module = "MEMORY") {
  snap <- .mem_snapshot()
  if (is.na(snap$gc_total_mb)) {
    .log_write("INFO", "Memory info not available on this platform.", module = module)
    return(invisible(NULL))
  }
  
  if (is.finite(snap$gc_total_mb)) {
    if (is.na(.mem_state$peak_gc_mb) || snap$gc_total_mb > .mem_state$peak_gc_mb) {
      .mem_state$peak_gc_mb <- snap$gc_total_mb
    }
  }
  if (is.finite(snap$rss_mb)) {
    if (is.na(.mem_state$peak_rss_mb) || snap$rss_mb > .mem_state$peak_rss_mb) {
      .mem_state$peak_rss_mb <- snap$rss_mb
    }
  }
  
  msg <- sprintf("Memory%s: gc≈%.1f MB (peak_gc≈%.1f MB)",
                 if (!is.null(label)) paste0(" [", label, "]") else "",
                 snap$gc_total_mb, .mem_state$peak_gc_mb)
  
  if (is.finite(snap$rss_mb)) {
    msg <- paste0(msg, sprintf(" | RSS≈%.1f MB", snap$rss_mb))
    if (is.finite(.mem_state$peak_rss_mb)) {
      msg <- paste0(msg, sprintf(" (peak_RSS≈%.1f MB)", .mem_state$peak_rss_mb))
    }
  }
  
  .log_write("INFO", msg, module = module)
  invisible(NULL)
}

with_log_section <- function(section_name, expr, log_mem = TRUE) {
  log_info("=== START: %s ===", section_name, module = section_name)
  if (log_mem) log_memory("start", module = section_name)
  
  on.exit({
    if (log_mem) log_memory("end", module = section_name)
    log_info("=== END: %s ===", section_name, module = section_name)
  }, add = TRUE)
  
  force(expr)
}
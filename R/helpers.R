# Shared file, checkpoint, progress, and separation utilities.

sha_file <- function(path) digest::digest(file = path, algo = "sha256")

sha_object <- function(x) digest::digest(x, algo = "sha256")

now_utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

read_json <- function(path) jsonlite::read_json(path, simplifyVector = TRUE)

atomic_json <- function(x, path) {
  tmp <- paste0(path, ".pending")
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, digits = 16, null = "null", na = "null")
  stopifnot(file.rename(tmp, path))
}

immutable_csv <- function(x, path) {
  if (file.exists(path)) {
    classes <- vapply(x, function(z) {
      if (is.integer(z)) "integer" else if (is.numeric(z)) "numeric" else if (is.logical(z)) "logical" else "character"
    }, "")
    old <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, colClasses = classes, na.strings = "")
    stopifnot(isTRUE(all.equal(old, x, check.attributes = FALSE, tolerance = 1e-12)))
    return(invisible(path))
  }
  write.csv(x, paste0(path, ".pending"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  stopifnot(file.rename(paste0(path, ".pending"), path)); invisible(path)
}

registry_path <- function(out) file.path(out, "checkpoints.json")

new_registry <- function(signature) list(signature = signature, packs = list())

read_registry <- function(out, signature) {
  reg <- jsonlite::read_json(registry_path(out), simplifyVector = FALSE)
  stopifnot(identical(reg$signature, signature)); reg
}

pack_read <- function(out, name, signature, ids = NULL, recover = FALSE) {
  path <- file.path(out, "packs", paste0(name, ".rds"))
  if (!file.exists(path)) return(NULL)
  reg <- read_registry(out, signature); receipt <- reg$packs[[name]]
  actual <- sha_file(path)
  if (!is.null(receipt)) stopifnot(identical(receipt$sha256, actual), receipt$bytes == file.info(path)$size)
  obj <- readRDS(path)
  stopifnot(identical(obj$signature, signature), identical(obj$name, name),
    identical(obj$payload_sha256, sha_object(obj$payload)), !anyDuplicated(obj$ids))
  if (!is.null(ids)) stopifnot(identical(obj$ids, ids))
  if (is.null(receipt)) {
    if (!recover) stop("Unregistered pack: run resume to validate and recover its atomic envelope.")
    reg$packs[[name]] <- list(sha256 = actual, bytes = unname(file.info(path)$size), ids = obj$ids)
    atomic_json(reg, registry_path(out))
  }
  obj$payload
}

pack_commit <- function(out, name, signature, ids, payload) {
  path <- file.path(out, "packs", paste0(name, ".rds"))
  if (file.exists(path)) {
    saved <- pack_read(out, name, signature, ids, recover = TRUE)
    stopifnot(identical(sha_object(saved), sha_object(payload))); return(invisible(payload))
  }
  obj <- list(signature = signature, name = name, ids = ids,
    payload_sha256 = sha_object(payload), payload = payload)
  saveRDS(obj, paste0(path, ".pending"), version = 3L)
  stopifnot(file.rename(paste0(path, ".pending"), path))
  pack_read(out, name, signature, ids, recover = TRUE)
  invisible(payload)
}

progress_writer <- function(out, phase, total, committed = 0L, failed = 0L, interval = 45) {
  start <- Sys.time(); last <- as.POSIXct(0, origin = "1970-01-01"); initial <- committed
  completed <- committed; recent <- NA_character_; last_completed <- NA_character_; durations <- numeric()
  previous_path <- file.path(out, "progress.json")
  if (file.exists(previous_path)) {
    previous <- read_json(previous_path)
    if (identical(previous$phase, phase) && !is.null(previous$last_completed_unit))
      last_completed <- previous$last_completed_unit
  }
  function(unit = recent, done = completed, saved = committed, errors = failed,
           duration = NULL, force = FALSE, state = "running") {
    if (!is.null(duration)) durations <<- c(durations, duration)
    if (done > completed) last_completed <<- unit
    completed <<- done; committed <<- saved; failed <<- errors; recent <<- unit
    if (!force && as.numeric(difftime(Sys.time(), last, units = "secs")) < interval) return(invisible(NULL))
    last <<- Sys.time(); elapsed <- as.numeric(difftime(last, start, units = "secs"))
    rate <- if (length(durations) >= 2L && sum(durations) > 0) length(durations) / sum(durations) else NA_real_
    eta <- if (is.finite(rate)) (total - done) / rate else NA_real_
    snap <- list(run_id = basename(out), pid = Sys.getpid(), phase = phase, state = state,
      total_units = total, validated_completed = done - errors,
      running = as.integer(state == "running" && done < total), failed = errors,
      pending = max(0L, total - done - as.integer(state == "running" && done < total)),
      durably_committed = saved, computed_units = done,
      completion_percent = 100 * (done - errors) / total, elapsed_seconds_this_session = elapsed,
      resumed_committed = initial, measured_units_per_second = rate,
      computational_eta_seconds_for_this_phase = eta,
      estimated_phase_finish_utc = if (is.finite(eta)) format(last + eta, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NA_character_,
      whole_run_eta = "unknown; class review, later stages and final verification excluded",
      heartbeat_utc = now_utc(), most_recent_unit = unit,
      last_completed_unit = last_completed)
    atomic_json(snap, file.path(out, "progress.json"))
    path <- file.path(out, "progress.tsv")
    row <- data.frame(time = now_utc(), phase = phase, state = state, total = total,
      validated = done - errors, failed = errors, committed = saved, computed = done,
      elapsed = elapsed, eta_seconds = eta, unit = unit)
    write.table(row, path, sep = "\t", row.names = FALSE, col.names = !file.exists(path), append = file.exists(path), quote = FALSE, na = "unknown")
    cat(sprintf("[%s] %s valid=%d/%d saved=%d failed=%d phase_ETA=%s sec unit=%s\n",
      now_utc(), phase, done - errors, total, saved, errors,
      if (is.finite(eta)) round(eta) else "estimating", unit)); flush.console()
    invisible(snap)
  }
}

tb_atomic <- function(object,path,replace=FALSE) {
  if(!replace && file.exists(path))stop("Refusing overwrite: ",path)
  tmp <- paste0(path,".pending")
  saveRDS(object,tmp,version=2,compress="gzip")
  stopifnot(file.rename(tmp,path));invisible(path)
}

separation_check <- function(X, y) {
  Z <- X * (2 * y - 1); p <- ncol(X)
  z <- lpSolve::lp("max", c(colSums(Z), -colSums(Z)) / nrow(Z),
                  rbind(cbind(Z, -Z), diag(2 * p)),
                  c(rep(">=", nrow(Z)), rep("<=", 2 * p)),
                  c(rep(0, nrow(Z)), rep(1, 2 * p)), timeout = 20L)
  list(status = z$status, objective = z$objval,
       separated = if (z$status == 0) z$objval > 1e-7 else NA)
}

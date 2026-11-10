source("R/helpers.R", local=.GlobalEnv)
source_paths <- function(base) sort(c(list.files(base,pattern="\\.(R|json)$",full.names=TRUE),
  "run.R", "R/helpers.R", "R/s12.R"),method="radix")
check_inputs <- function(cfg,synthetic=FALSE) {
  stopifnot(as.character(getRversion())==cfg$runtime$R)
  for(p in setdiff(names(cfg$runtime),"R")) stopifnot(requireNamespace(p,quietly=TRUE),as.character(packageVersion(p))==cfg$runtime[[p]])
  if(synthetic)return(invisible(TRUE))
  for(f in names(cfg$raw_sha256)) if(!identical(sha_file(f),cfg$raw_sha256[[f]]))stop("Raw checksum mismatch: ",f)
}
fingerprint <- function(files) data.frame(file=files,bytes=as.numeric(file.info(files)$size),sha256=vapply(files,sha_file,""),row.names=NULL)
same_manifest <- function(a,b) {
  valid <- function(x) is.data.frame(x) && identical(names(x),c("file","bytes","sha256")) &&
    !anyNA(x) && !anyDuplicated(x$file) && all(nzchar(x$file)) &&
    is.numeric(x$bytes) && all(is.finite(x$bytes) & x$bytes>=0) &&
    all(grepl("^[0-9a-f]{64}$",x$sha256))
  if(!valid(a) || !valid(b) || !setequal(a$file,b$file))return(FALSE)
  # File identity, size and checksum matter; locale-dependent row order does not.
  i<-match(a$file,b$file)
  identical(as.numeric(a$bytes),as.numeric(b$bytes[i])) && identical(a$sha256,b$sha256[i])
}
context <- function(base,cfg,synthetic=FALSE) {
  check_inputs(cfg,synthetic)
  dat<-if(synthetic)synthetic_data(cfg$synthetic_seed)else load_real_data(cfg)
  details<-list(config=cfg,code=fingerprint(source_paths(base)),raw=fingerprint(if(synthetic)character()else names(cfg$raw_sha256)),
    runtime=cfg$runtime,R_version=R.version.string,platform=R.version$platform,
    analysis_type=if(synthetic)"SYNTHETIC_VALIDATION_ONLY"else"REAL_DATA_USER_EXECUTION",
    dataset_sha256=sha_object(dat$sets),plan=model_plan())
  list(base=base,cfg=cfg,sets=dat$sets,flow=dat$flow,sizes=dat$sizes,plan=model_plan(),details=details,signature=sha_object(details))
}
assert_provenance <- function(ctx) {
  check_inputs(ctx$cfg,synthetic=!nrow(ctx$details$raw))
  stopifnot(identical(fingerprint(ctx$details$code$file),ctx$details$code),identical(fingerprint(ctx$details$raw$file),ctx$details$raw))
}
check_freeze <- function(base) {
  path<-file.path(base,"FREEZE.tsv");if(!file.exists(path))stop("Missing FREEZE.tsv; validated source freeze is required")
  f<-read.delim(path,stringsAsFactors=FALSE)
  actual<-fingerprint(source_paths(base))
  if(!same_manifest(f,actual)) {
    missing<-setdiff(f$file,actual$file);extra<-setdiff(actual$file,f$file)
    common<-match(f$file,actual$file);present<-!is.na(common)
    changed<-f$file[present][f$bytes[present]!=actual$bytes[common[present]] |
      f$sha256[present]!=actual$sha256[common[present]]]
    stop(paste(c("Source freeze mismatch; execution stopped before model fitting.",
      if(length(missing))paste("Missing:",paste(missing,collapse=", ")),
      if(length(extra))paste("Unexpected:",paste(extra,collapse=", ")),
      if(length(changed))paste("Changed:",paste(changed,collapse=", ")),
      "The manifest must also contain unique, complete file/bytes/SHA-256 records."),collapse="\n"),call.=FALSE)
  }
  invisible(TRUE)
}
initialize_run <- function(out,ctx) {
  if(dir.exists(out))stop("Output exists. Use resume; never overwrite: ",out)
  stopifnot(dir.create(file.path(out,"private","packs"),recursive=TRUE))
  tb_atomic(list(signature=ctx$signature,details=ctx$details),file.path(out,"private","metadata.rds"))
  tb_atomic(ctx$sets,file.path(out,"private","prepared.rds"))
  atomic_json(new_registry(ctx$signature),file.path(out,"private","checkpoints.json"))
  immutable_csv(ctx$plan,file.path(out,"work_units.csv"));immutable_csv(ctx$sizes,file.path(out,"sample_sizes.csv"))
  immutable_csv(ctx$flow,file.path(out,"sample_flow.csv"));immutable_csv(ctx$details$code,file.path(out,"source_manifest.csv"))
  immutable_csv(ctx$details$raw,file.path(out,"raw_manifest.csv"))
  capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
  writeLines(c(ctx$details$analysis_type,"Private RDS objects include survey identifiers; aggregate tables do not.",
    "One work unit = one fixed model plus numerical acceptance checks. Total = 16.",
    "Raw inputs unchanged. No stochastic production algorithm; synthetic seed is in config."),file.path(out,"RUN_SCOPE.txt"))
}
check_metadata <- function(out,ctx) {
  meta<-readRDS(file.path(out,"private","metadata.rds"))
  stopifnot(identical(meta$signature,ctx$signature),identical(meta$details,ctx$details),
    identical(sha_object(readRDS(file.path(out,"private","prepared.rds"))),sha_object(ctx$sets)))
}
read_pack <- function(out,ctx,batch,recover=FALSE) {
  store<-file.path(out,"private");name<-sprintf("pack%02d",batch)
  path<-file.path(store,"packs",paste0(name,".rds"));if(!file.exists(path))return(NULL)
  rows<-ctx$plan[ctx$plan$pack==batch,,drop=FALSE];obj<-readRDS(path)
  stopifnot(identical(obj$signature,ctx$signature),identical(obj$name,name),identical(obj$ids,rows$id),
    identical(obj$payload_sha256,sha_object(obj$payload)),length(obj$payload)==nrow(rows))
  # Scientific replay precedes adoption of a rename-before-registry checkpoint.
  for(j in seq_len(nrow(rows)))validate_record(obj$payload[[j]],as.list(rows[j,,drop=FALSE]),ctx$sets,ctx$cfg)
  pack_read(store,name,ctx$signature,rows$id,recover=recover)
}
all_records <- function(out,ctx,recover=FALSE) {
  disk<-list.files(file.path(out,"private","packs"),pattern="\\.rds$")
  stopifnot(all(disk %in% sprintf("pack%02d.rds",1:4)))
  reg<-read_registry(file.path(out,"private"),ctx$signature)
  registered_files<-if(length(reg$packs))paste0(names(reg$packs),".rds")else character()
  stopifnot(all(names(reg$packs) %in% sprintf("pack%02d",1:4)),all(registered_files %in% disk))
  z<-list();missing<-FALSE
  for(b in 1:4) {
    p<-read_pack(out,ctx,b,recover);if(is.null(p))missing<-TRUE else {stopifnot(!missing);z<-c(z,p)}
  };z
}
acquire_writer <- function(out,ctx) {
  lock<-file.path(out,"private","writer.lock")
  if(!dir.create(lock,showWarnings=FALSE))stop("Writer lock exists. If the old process ended, use unlock.")
  tb_atomic(list(pid=Sys.getpid(),host=Sys.info()[["nodename"]],signature=ctx$signature),file.path(lock,"owner.rds"));lock
}
retire_writer <- function(out,lock) {
  if(!dir.exists(lock))return(invisible(NULL))
  history<-file.path(out,"private","lock_history");dir.create(history,showWarnings=FALSE)
  target<-file.path(history,paste0("ended_",Sys.getpid(),"_",format(Sys.time(),"%Y%m%dT%H%M%S")))
  stopifnot(!file.exists(target),file.rename(lock,target));invisible(NULL)
}
unlock_writer <- function(out) {
  lock<-file.path(out,"private","writer.lock");owner<-readRDS(file.path(lock,"owner.rds"))
  stopifnot(owner$host==Sys.info()[["nodename"]],!isTRUE(suppressWarnings(tools::pskill(owner$pid,signal=0L))))
  retire_writer(out,lock);cat("Ended writer lock retained in private/lock_history.\n")
}
summary_tables <- function(records) {
  valid<-Filter(function(r)r$status=="valid",records)
  co<-if(length(valid))do.call(rbind,lapply(valid,`[[`,"coefficients"))else data.frame(
    model=character(),sample=character(),outcome=character(),kind=character(),term=character(),
    log_OR=numeric(),SE=numeric(),OR=numeric(),lower95=numeric(),upper95=numeric(),p=numeric(),df=numeric(),
    pairs=integer(),mothers=integer(),households=integer(),weighted=logical())
  status<-do.call(rbind,lapply(records,function(r)data.frame(model=r$id,sample=r$sample,status=r$status,
    error=if(is.null(r$error))NA_character_ else r$error,elapsed_seconds=r$elapsed_seconds)))
  effect<-co[co$term=="reasonfamily" | (co$model=="H3_reason_by_sex" & co$term=="reasonfamily:sexgirl"),,drop=FALSE]
  effect<-effect[!(effect$model=="H3_reason_by_sex" & effect$term=="reasonfamily"),,drop=FALSE]
  secondary_ids<-c("H2_less_than_mother","H3_reason_by_sex","secondary_woman_job","secondary_man_provider")
  secondary<-effect[effect$model %in% secondary_ids,,drop=FALSE]
  secondary$p_Holm_four_planned<-p.adjust(secondary$p,method="holm",n=4L)
  thresholds<-effect[grepl("^threshold_",effect$model),,drop=FALSE]
  list(model_status=status,coefficients=co,effects=effect,secondary=secondary,thresholds=thresholds)
}
make_report <- function(out,ctx,records,tables) {
  p<-tables$effects[tables$effects$model=="M3",,drop=FALSE]
  lines<-c("TAYA EGITIM ENGELI: HESAPLAMA RAPORU",ctx$details$analysis_type,
    paste("Imza:",ctx$signature),paste("Gecerli model:",sum(vapply(records,function(r)r$status=="valid",TRUE)),"/ 16"),
    "OR>1 KADIN_EVISLERI/ERKEK_GECIM icin daha geleneksel, KADIN_IS icin kadinin isi olmasina daha fazla katilim demektir.",
    "H2 OR>1 cocugun anneden daha dusuk geleneksellik yaniti vermesi; H3 OR etkilesimidir.",
    "Hane-kumeli yaklasik tasarim kullanildi; ozgun TUIK PSU/tabaka kodlari mevcut degil.",
    "Nedensel etki, pismanlik veya psikolojik mekanizma dogrudan tanimlanmamistir.",
    "Esik modelleri orantili odds tanisidir; bagimsiz testler/resmi varsayim kaniti degildir.",
    "Anlamsiz sonuc esitlik kaniti degildir.","")
  if(nrow(p))lines<-c(lines,sprintf("BIRINCIL M3: OR=%.4f; %%95 GA=[%.4f, %.4f]; log-OR SE=%.4f; p=%.5g; n=%d; hane=%d.",p$OR,p$lower95,p$upper95,p$SE,p$p,p$pairs,p$households))
  else lines<-c(lines,"BIRINCIL MODEL KABUL EDILMEDI: katsayi yorumlanmamali; model_status.csv incelenmeli.")
  for(n in c("effects","secondary","thresholds","model_status")) {
    z<-tables[[n]];if(ncol(z))lines<-c(lines,"",toupper(n),knitr::kable(z,format="pipe",digits=4,row.names=FALSE))
  }
  path<-file.path(out,"SONUCLAR.txt")
  if(file.exists(path))stopifnot(identical(readLines(path,warn=FALSE),as.character(lines)))else writeLines(lines,path,useBytes=TRUE)
}
immutable_json <- function(x,path) {
  if(file.exists(path)) {
    target<-jsonlite::fromJSON(jsonlite::toJSON(x,auto_unbox=TRUE,digits=16,null="null",na="null"),simplifyVector=FALSE)
    stopifnot(identical(jsonlite::read_json(path,simplifyVector=FALSE),target))
  }else atomic_json(x,path)
}
manifest_paths <- function(out) {
  p<-list.files(out,recursive=TRUE,full.names=TRUE);r<-substring(p,nchar(out)+2L)
  keep<-!grepl("^progress\\.|^manifest\\.tsv$|^private/(writer.lock|lock_history)/|\\.pending$",r)
  sort(p[keep],method="radix")
}
write_manifest <- function(out) {
  files<-manifest_paths(out);m<-fingerprint(files);m$file<-substring(m$file,nchar(out)+2L)
  stopifnot(sum(file.info(list.files(out,recursive=TRUE,full.names=TRUE))$size)<=50*1024^2)
  path<-file.path(out,"manifest.tsv")
  if(file.exists(path)) {stopifnot(same_manifest(read.delim(path,stringsAsFactors=FALSE),m));return(invisible(NULL))}
  tmp<-file.path(out,"manifest.tsv.pending");write.table(m,tmp,sep="\t",row.names=FALSE,quote=FALSE)
  stopifnot(file.rename(tmp,file.path(out,"manifest.tsv")))
}
finish_run <- function(out,ctx,records) {
  stopifnot(length(records)==16L,identical(vapply(records,`[[`,"","id"),ctx$plan$id))
  tables<-summary_tables(records)
  for(n in names(tables))immutable_csv(tables[[n]],file.path(out,paste0(n,".csv")))
  immutable_csv(comparison_table(ctx$sets$primary),file.path(out,"comparison_unweighted.csv"))
  immutable_csv(observed_table(ctx$sets$primary),file.path(out,"observed_categories.csv"))
  immutable_json(lapply(records,`[[`,"audit"),file.path(out,"diagnostics.json"))
  valid<-vapply(records,function(r)r$status=="valid",TRUE)
  summary<-list(signature=ctx$signature,analysis_type=ctx$details$analysis_type,
    expected_units=16L,recorded_units=length(records),valid_units=sum(valid),rejected_units=sum(!valid),
    primary_valid=valid[4],all_models_valid=all(valid),complete_state=if(all(valid))"ALL_NUMERICAL_GATES_PASSED"else"COMPUTATION_FINISHED_REVIEW_REQUIRED",
    original_survey_PSU_available=FALSE,household_cluster_approximation=TRUE,
    population_scope="Co-resident young adults in the two maternal interruption-reason groups",
    raw_inputs_unchanged=TRUE,production_RNG_used=FALSE)
  immutable_json(summary,file.path(out,"summary.json"));make_report(out,ctx,records,tables)
  assert_provenance(ctx);write_manifest(out);invisible(summary)
}
verify_run <- function(out,ctx,strict=TRUE) {
  check_metadata(out,ctx);assert_provenance(ctx);z<-all_records(out,ctx,FALSE)
  stopifnot(length(z)==16L,identical(vapply(z,`[[`,"","id"),ctx$plan$id))
  m<-read.delim(file.path(out,"manifest.tsv"),stringsAsFactors=FALSE);files<-manifest_paths(out)
  actual<-fingerprint(files);actual$file<-substring(actual$file,nchar(out)+2L)
  stopifnot(!anyDuplicated(m$file),same_manifest(actual,m))
  tables<-summary_tables(z);for(n in names(tables))immutable_csv(tables[[n]],file.path(out,paste0(n,".csv")))
  s<-jsonlite::read_json(file.path(out,"summary.json"),simplifyVector=TRUE)
  good<-vapply(z,function(r)r$status=="valid",TRUE)
  stopifnot(s$signature==ctx$signature,s$recorded_units==16L,s$valid_units==sum(good),s$rejected_units==sum(!good),identical(s$all_models_valid,all(good)),identical(s$primary_valid,good[4]))
  cat("PASS integrity: 16 fixed records, replayed numerical gates, matching raw/code/sample/runtime signatures and manifests.\n")
  if(strict && !all(good))stop("Integrity passed, but rejected model records require review: ",paste(ctx$plan$id[!good],collapse=", "))
  invisible(s)
}
execute_run <- function(out,ctx,start=FALSE,stop_after=Inf) {
  if(start)initialize_run(out,ctx)
  check_metadata(out,ctx);lock<-acquire_writer(out,ctx);on.exit(retire_writer(out,lock),add=TRUE)
  saved<-all_records(out,ctx,TRUE);done<-length(saved);failed<-sum(vapply(saved,function(r)r$status=="rejected",TRUE))
  progress<-progress_writer(out,"fixed_model_analysis",16L,committed=done,failed=failed,interval=ctx$cfg$progress_seconds)
  progress(unit="starting",done=done,saved=done,errors=failed,force=TRUE)
  for(b in 1:4) {
    rows<-ctx$plan[ctx$plan$pack==b,,drop=FALSE];if(rows$index[4]<=length(saved))next
    buffer<-list()
    for(j in 1:4) {
      t<-as.list(rows[j,,drop=FALSE]);r<-fit_task(t,ctx$sets,ctx$cfg);validate_record(r,t,ctx$sets,ctx$cfg)
      buffer[[j]]<-r;done<-done+1L;failed<-failed+as.integer(r$status=="rejected")
      progress(unit=r$id,done=done,saved=length(saved),errors=failed,duration=r$elapsed_seconds,force=TRUE)
    }
    assert_provenance(ctx);pack_commit(file.path(out,"private"),sprintf("pack%02d",b),ctx$signature,rows$id,buffer)
    saved<-c(saved,buffer);progress(unit=rows$id[4],done=done,saved=length(saved),errors=failed,force=TRUE)
    if(length(saved)>=stop_after && length(saved)<16L)return(invisible(list(state="VALIDATION_CHECKPOINT_STOP",saved=length(saved))))
  }
  summary<-finish_run(out,ctx,saved);verify_run(out,ctx,strict=FALSE)
  progress(unit="verified",done=16L,saved=16L,errors=failed,force=TRUE,state=if(failed)"review_required"else"complete")
  invisible(summary)
}

verify_expected_primary <- function(out, verified) {
  status <- read.csv(file.path(out, "model_status.csv"))
  expected_rejections <- c("threshold_gt1", "threshold_gt4")
  if (!isTRUE(verified$primary_valid) || verified$valid_units != 14L ||
      !identical(status$model[status$status == "rejected"], expected_rejections))
    stop("Primary run differs from the expected 14 accepted fits and two separation diagnostics.")
  cat("PASS recorded primary result: 14 accepted fits and the two expected extreme-threshold rejections.\n")
  invisible(verified)
}

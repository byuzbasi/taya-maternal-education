h3_initialize <- function(out,ctx) {
  if(dir.exists(out))stop("Output exists; use resume. Refusing overwrite: ",out)
  stopifnot(dir.create(file.path(out,"private","packs"),recursive=TRUE))
  tb_atomic(list(signature=ctx$signature,details=ctx$details),file.path(out,"private","metadata.rds"))
  tb_atomic(ctx$sets,file.path(out,"private","prepared.rds"))
  tb_atomic(ctx$reference,file.path(out,"private","reference.rds"))
  atomic_json(new_registry(ctx$signature),file.path(out,"private","checkpoints.json"))
  immutable_csv(ctx$plan,file.path(out,"work_units.csv"));immutable_csv(ctx$sizes,file.path(out,"sample_sizes.csv"))
  immutable_csv(ctx$flow,file.path(out,"sample_flow.csv"));immutable_csv(ctx$details$code,file.path(out,"source_manifest.csv"))
  immutable_csv(ctx$details$raw,file.path(out,"raw_manifest.csv"))
  if(length(ctx$details$baseline))immutable_csv(ctx$details$baseline,file.path(out,"reference_manifest.csv"))
  capture.output(sessionInfo(),file=file.path(out,"session_info.txt"))
  writeLines(c(ctx$details$analysis_type,"Four new fixed H3 models; one signed checkpoint pack per model.",
    "Reference H3 is reused from existing results in a real run. All new real fits are user-executed.",
    "Private files include identifiers. No raw data or previous results are modified.",
    "No production RNG. Synthetic seed is in config. Approximate household design; original PSU/strata unavailable."),
    file.path(out,"RUN_SCOPE.txt"))
}
h3_check_metadata <- function(out,ctx) {
  core$check_metadata(out,ctx)
  stopifnot(identical(sha_object(readRDS(file.path(out,"private","reference.rds"))),sha_object(ctx$reference)))
}
h3_report <- function(out,ctx,records,tables) {
  lines<-c("TAYA H3: DORT DUYARLILIK KONTROLU",ctx$details$analysis_type,
    paste("Imza:",ctx$signature),paste("Gecerli yeni model:",sum(vapply(records,function(r)r$status=="valid",TRUE)),"/ 4"),
    "Orijinal H3 ikincil/kesfedicidir. Yeni duyarliliklar sonuclar goruldukten sonra planlanmistir.",
    "Orijinal H3 Holm p'si ilk dort ikincil sinama ailesinden korunur.",
    "Yeni dort etkilesim icin ayri Holm(4) raporlanir; orijinal H3'un yerine gecmez.",
    "OR etkilesimi, kizlardaki neden OR'sinin ogullardaki neden OR'sine oranidir.",
    "Alt grup ve etkilesim GA'lari coklu sinamaya gore duzeltilmemistir.",
    "Olasiliklar her model ornekleminin ortak havuzlanmis kovaryat dagiliminda standartlastirilmis nokta tahminleridir.",
    "Olasiliklar nedensel degildir; olasiliklar icin ayri test veya GA uretilemez.",
    "Etkili hane tanisi dogrusallastirilmis katkidir; hane-silme tahmini veya otomatik dislama degildir.",
    "Ayni TAYA verisindeki ortusen duyarliliklar bagimsiz tekrarlar degildir; p degerleri birlestirilmez.",
    "Onceki iki uc esik ayrismasi cozulmus sayilmaz. Ortak OR yorumu orantili odds varsayimina kosulludur.",
    "Pismanlik, nedensel aktarim veya tum Turkiye gencligine genelleme dogrudan tanimlanmamistir.")
  for(n in c("model_status","interactions","contrasts","influence"))
    lines<-c(lines,"",toupper(n),knitr::kable(tables[[n]],format="pipe",digits=4,row.names=FALSE))
  p<-file.path(out,"SONUCLAR.txt")
  if(file.exists(p))stopifnot(identical(readLines(p,warn=FALSE),as.character(lines)))else writeLines(lines,p,useBytes=TRUE)
}
h3_finish <- function(out,ctx,records) {
  stopifnot(length(records)==4L,identical(vapply(records,`[[`,"","id"),ctx$plan$id))
  tables<-h3_tables(records,ctx)
  for(n in names(tables))immutable_csv(tables[[n]],file.path(out,paste0(n,".csv")))
  core$immutable_json(lapply(records,`[[`,"audit"),file.path(out,"diagnostics.json"))
  good<-vapply(records,function(r)r$status=="valid",TRUE)
  summary<-list(signature=ctx$signature,analysis_type=ctx$details$analysis_type,
    expected_new_units=4L,recorded_new_units=length(records),valid_new_units=sum(good),
    rejected_new_units=sum(!good),all_new_models_valid=all(good),
    complete_state=if(all(good))"ALL_NEW_NUMERICAL_GATES_PASSED"else"COMPUTATION_FINISHED_REVIEW_REQUIRED",
    reference_original_Holm=ctx$details$reference_Holm,reference_reused=TRUE,
    confirmatory_replication=FALSE,original_PSU_available=FALSE,
    raw_inputs_unchanged=TRUE,previous_outputs_unchanged=TRUE,production_RNG_used=FALSE)
  core$immutable_json(summary,file.path(out,"summary.json"));h3_report(out,ctx,records,tables)
  h3_provenance(ctx);core$write_manifest(out);invisible(summary)
}
h3_verify <- function(out,ctx,strict=TRUE) {
  h3_check_metadata(out,ctx);h3_provenance(ctx);records<-core$all_records(out,ctx,FALSE)
  stopifnot(length(records)==4L,identical(vapply(records,`[[`,"","id"),ctx$plan$id))
  m<-read.delim(file.path(out,"manifest.tsv"),stringsAsFactors=FALSE)
  actual<-core$fingerprint(core$manifest_paths(out));actual$file<-substring(actual$file,nchar(out)+2L)
  stopifnot(core$same_manifest(m,actual))
  tables<-h3_tables(records,ctx)
  for(n in names(tables))immutable_csv(tables[[n]],file.path(out,paste0(n,".csv")))
  good<-vapply(records,function(r)r$status=="valid",TRUE)
  s<-jsonlite::read_json(file.path(out,"summary.json"),simplifyVector=TRUE)
  stopifnot(s$signature==ctx$signature,s$recorded_new_units==4L,
    s$valid_new_units==sum(good),s$rejected_new_units==sum(!good),
    identical(s$all_new_models_valid,all(good)))
  h3_report(out,ctx,records,tables)
  cat("PASS H3 integrity: 4 fixed records, numerical/contrast/probability/influence replay, signatures and manifests.\n")
  if(strict && !all(good))stop("Rejected H3 models require review: ",paste(ctx$plan$id[!good],collapse=", "))
  invisible(s)
}
h3_execute <- function(out,ctx,start=FALSE,stop_after=Inf) {
  if(start)h3_initialize(out,ctx)
  h3_check_metadata(out,ctx);lock<-core$acquire_writer(out,ctx);on.exit(core$retire_writer(out,lock),add=TRUE)
  saved<-core$all_records(out,ctx,TRUE);done<-length(saved)
  failed<-sum(vapply(saved,function(r)r$status=="rejected",TRUE))
  progress<-progress_writer(out,"H3_fixed_sensitivity_models",4L,committed=done,failed=failed,interval=ctx$cfg$progress_seconds)
  progress(unit="starting",done=done,saved=done,errors=failed,force=TRUE)
  for(i in seq_len(4L)) {
    if(i<=length(saved))next
    task<-as.list(ctx$plan[i,,drop=FALSE]);started<-Sys.time();r<-h3_fit(task,ctx$sets,ctx$cfg)
    h3_validate(r,task,ctx$sets,ctx$cfg)
    r$elapsed_seconds<-as.numeric(difftime(Sys.time(),started,units="secs"))
    done<-i;failed<-failed+as.integer(r$status=="rejected")
    progress(unit=r$id,done=done,saved=length(saved),errors=failed,duration=r$elapsed_seconds,force=TRUE)
    h3_provenance(ctx)
    pack_commit(file.path(out,"private"),sprintf("pack%02d",i),ctx$signature,task$id,list(r))
    saved<-c(saved,list(r));progress(unit=r$id,done=done,saved=length(saved),errors=failed,force=TRUE)
    if(done>=stop_after && done<4L)return(invisible(list(state="SYNTHETIC_CHECKPOINT_STOP",saved=done)))
  }
  summary<-h3_finish(out,ctx,saved);h3_verify(out,ctx,strict=FALSE)
  progress(unit="verified",done=4L,saved=4L,errors=failed,force=TRUE,state=if(failed)"review_required"else"complete")
  invisible(summary)
}
h3_preflight <- function(ctx) {
  for(i in seq_len(nrow(ctx$plan))) {
    task<-as.list(ctx$plan[i,,drop=FALSE]);d<-droplevels(ctx$sets[[task$sample]])
    z<-h3_design(task,d);X<-model.matrix(z$formula,z$data)
    stopifnot(qr(X)$rank==ncol(X),all(complete.cases(model.frame(z$formula,z$data,na.action=na.pass))),
      all(c("reasonfamily","sexgirl","reasonfamily:sexgirl") %in% colnames(X)),all(table(d$reason,d$sex)>0))
    cat(task$id,"pairs=",nrow(d),"mothers=",length(unique(d$mother_id)),
      "rank=",qr(X)$rank,"alias=",paste(z$alias,collapse=","),"\n")
  }
  h3_provenance(ctx);cat("PASS H3 sample/design preflight; no new real-data model fitted.\n")
}

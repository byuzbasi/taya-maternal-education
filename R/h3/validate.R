h3_reload_synthetic <- function(base,cfg,out) {
  meta<-readRDS(file.path(out,"private","metadata.rds"))
  stopifnot(meta$details$analysis_type=="SYNTHETIC_VALIDATION_ONLY",
    identical(meta$signature,sha_object(meta$details)),identical(meta$details$config,cfg),
    identical(meta$details$code,core$fingerprint(h3_source_paths(base,cfg))))
  sets<-readRDS(file.path(out,"private","prepared.rds"));reference<-readRDS(file.path(out,"private","reference.rds"))
  stopifnot(identical(sha_object(sets),meta$details$dataset_sha256),
    identical(sha_object(reference$coefficients),meta$details$reference_coefficients_sha256))
  list(base=base,cfg=cfg,sets=sets,reference=reference,plan=h3_plan(),
    details=meta$details,signature=meta$signature)
}
h3_validation <- function(base,cfg,out) {
  started<-Sys.time();stopifnot(!dir.exists(out))
  frozen<-core$fingerprint(h3_source_paths(base,cfg))
  stopifnot(core$same_manifest(frozen,frozen[rev(seq_len(nrow(frozen))),]))
  oldlocale<-Sys.getlocale("LC_COLLATE");Sys.setlocale("LC_COLLATE","C")
  stopifnot(identical(frozen,core$fingerprint(h3_source_paths(base,cfg))))
  Sys.setlocale("LC_COLLATE",oldlocale)
  ctx<-h3_context(base,cfg,synthetic=TRUE);h3_preflight(ctx)
  task<-as.list(ctx$plan[2,,drop=FALSE]);setup<-h3_design(task,droplevels(ctx$sets$shared))
  stopifnot(identical(setup$alias,"interruptionhigher"))
  cat("SYNTHETIC SCOPE: 260-household generator; shared-cell exact-alias fixture; fixed seed.\n")
  partial<-h3_execute(out,ctx,TRUE,stop_after=1L)
  stopifnot(partial$saved==1L,length(core$all_records(out,ctx))==1L)
  first<-sha_file(file.path(out,"private/packs/pack01.rds"))
  core$expect_failure(function()h3_initialize(out,ctx),"start refuses existing output")
  changed<-ctx;changed$signature<-paste0("wrong_",ctx$signature)
  core$expect_failure(function()h3_check_metadata(out,changed),"changed signature refuses checkpoint reuse")
  result<-h3_execute(out,ctx,FALSE)
  stopifnot(identical(first,sha_file(file.path(out,"private/packs/pack01.rds"))))
  if(!isTRUE(result$all_new_models_valid)) {
    print(read.csv(file.path(out,"model_status.csv")),row.names=FALSE)
    stop("Synthetic gate failure; real production has not been validated")
  }
  records<-core$all_records(out,ctx)
  bad<-records[[1]];bad$h3$contrasts$OR[1]<-bad$h3$contrasts$OR[1]*2
  core$expect_failure(function()h3_validate(bad,as.list(ctx$plan[1,,drop=FALSE]),ctx$sets,cfg),
    "tampered derived contrast rejected")
  for(i in seq_len(4L)) {
    r<-records[[i]];t<-as.list(ctx$plan[i,,drop=FALSE]);d<-droplevels(ctx$sets[[t$sample]])
    ctab<-r$h3$contrasts
    stopifnot(abs(ctab$OR[2]/ctab$OR[1]-ctab$OR[3])<1e-10)
    # Independent cumulative-logistic calculation checks predict() and pooled averaging.
    setup<-h3_design(t,d);nd<-setup$data
    nd$reason<-factor(rep("family",nrow(nd)),levels(d$reason))
    nd$sex<-factor(rep("girl",nrow(nd)),levels(d$sex))
    X<-model.matrix(delete.response(r$fit$terms),nd,contrasts.arg=r$fit$contrasts)
    X<-X[,names(r$fit$coefficients),drop=FALSE];eta<-drop(X%*%r$fit$coefficients)
    cp<-vapply(r$fit$zeta,function(z)plogis(z-eta),numeric(nrow(d)))
    pr<-cbind(cp,1)-cbind(0,cp);w<-if(t$weighted)d$weight else rep(1,nrow(d))
    expected<-colSums(pr*w)/sum(w)
    actual<-subset(r$h3$probabilities,reason=="family" & sex=="girl")$probability
    stopifnot(max(abs(expected-actual))<1e-12)
    d$weight<-d$weight*7
    stopifnot(isTRUE(all.equal(r$h3$probabilities,h3_probabilities(r$fit,t,d),tolerance=1e-12)))
  }
  dropped<-records;dropped[[2]]$status<-"rejected";dropped[[2]]$error<-"SYNTHETIC_REJECTION_FIXTURE"
  tab<-h3_tables(dropped,ctx)$interactions;sel<-tab$model!=cfg$reference_model
  stopifnot(isTRUE(all.equal(tab$p_Holm_new_four[sel],p.adjust(tab$p_unadjusted[sel],"holm",n=4L))))
  h3_verify(out,ctx)
  stopifnot(!dir.exists(cfg$output))
  cat("PASS: four synthetic fits, alias/interaction preservation, one-model checkpoint resume,\n",
    "source-locale stability, derived contrast tamper refusal, probability replay/weight scaling,\n",
    "household influence covariance replay and Holm family retained after rejection.\n")
  cat("VALIDATION OUTPUT:",out,"\nElapsed seconds:",round(as.numeric(difftime(Sys.time(),started,units="secs")),2),"\n")
  invisible(out)
}

# Existing scientific and storage functions remain unchanged on disk.
core <- new.env(parent=.GlobalEnv)
for(f in c("data.R","models.R","workflow.R","validate.R"))
  sys.source(file.path("R/primary",f),core)
original_design <- core$estimable_design
original_validation <- core$validate_record

h3_plan <- function() {
  p<-core$model_plan();p<-p[match(c("sensitivity_age15_29","sensitivity_shared_education",
    "sensitivity_link_flags","sensitivity_unweighted"),p$id),,drop=FALSE]
  p$id<-c("H3_age15_29","H3_shared_education","H3_link_flags","H3_unweighted")
  p$rhs<-paste(p$rhs,"+ reason:sex");p$pack<-1:4;p$index<-1:4;row.names(p)<-NULL;p
}
h3_design <- function(task,d) {
  if(task$id!="H3_shared_education")return(original_design(task,d))
  f<-core$model_formula(task);X<-model.matrix(f,d);q<-qr(X);alias<-character()
  if(q$rank<ncol(X)) {
    alias<-colnames(X)[q$pivot[seq.int(q$rank+1L,ncol(X))]]
    stopifnot(identical(alias,"interruptionhigher"),
      all(X[,alias]==X[,"mother_educationhigh"]+X[,"mother_educationhigher"]))
    keep<-setdiff(colnames(X),alias)
    stopifnot(all(c("reasonfamily","sexgirl","reasonfamily:sexgirl") %in% keep))
    nuisance<-setdiff(keep,c("(Intercept)","reasonfamily","reasonfamily:sexgirl"))
    for(n in setdiff(nuisance,"sexgirl"))d[[n]]<-X[,n]
    f<-reformulate(c("reason",ifelse(nuisance=="sexgirl","sex",nuisance),"reason:sex"),
      response=task$outcome,env=baseenv())
    Z<-model.matrix(f,d)
    stopifnot(qr(Z)$rank==ncol(Z),qr(cbind(Z,X))$rank==ncol(Z),identical(colnames(Z),keep))
  }
  list(data=d,formula=f,alias=alias,original_columns=ncol(X),original_rank=q$rank)
}
core$estimable_design <- h3_design

h3_source_paths <- function(base,cfg) sort(unique(c(
  list.files(base,pattern="\\.(R|json)$",full.names=TRUE), cfg$published_secondary,
  core$source_paths(cfg$original_base),file.path(cfg$original_base,"FREEZE.tsv"))),method="radix")
h3_freeze <- function(base,cfg) {
  f<-file.path(base,"FREEZE.tsv");if(!file.exists(f))stop("Missing validated H3 source freeze")
  if(!core$same_manifest(read.delim(f,stringsAsFactors=FALSE),core$fingerprint(h3_source_paths(base,cfg))))
    stop("H3 source freeze mismatch; no new model was fitted. Inspect source names, sizes and SHA-256.")
  core$check_freeze(cfg$original_base);invisible(TRUE)
}
h3_context <- function(base,cfg,synthetic=FALSE) {
  core$check_inputs(cfg,synthetic)
  if(synthetic) {
    dat<-core$synthetic_data(cfg$synthetic_seed)
    # Synthetic-only fixture for the same shared-cell nuisance identity.
    d<-dat$sets$primary
    d$m_EGITIM_YARIM_SEVIYE<-ifelse(as.character(d$mother_education) %in% c("high","higher"),
      "51",ifelse(d$m_EGITIM_YARIM_SEVIYE %in% c("51","53"),"41",d$m_EGITIM_YARIM_SEVIYE))
    dat$sets$shared<-core$build_fields(d);dat$sizes<-core$sample_table(dat$sets)
    rp<-core$model_plan();task<-as.list(rp[rp$id==cfg$reference_model,,drop=FALSE])
    reference<-core$fit_task(task,dat$sets,cfg)
    stopifnot(reference$status=="valid");original_validation(reference,task,dat$sets,cfg)
    baseline<-NULL
    reference_Holm<-NA_real_
  }else {
    oldcfg<-jsonlite::read_json(file.path(cfg$original_base,"config.json"),simplifyVector=TRUE)
    stopifnot(oldcfg$output==cfg$reference_output)
    old<-core$context(cfg$original_base,oldcfg)
    status<-core$verify_run(cfg$reference_output,old,strict=FALSE)
    core$verify_expected_primary(cfg$reference_output,status)
    saved<-core$all_records(cfg$reference_output,old,FALSE)
    reference<-saved[[which(old$plan$id==cfg$reference_model)]]
    stopifnot(reference$status=="valid")
    dat<-list(sets=old$sets,flow=old$flow,sizes=old$sizes)
    baseline<-core$fingerprint(c(core$manifest_paths(cfg$reference_output),file.path(cfg$reference_output,"manifest.tsv")))
    s<-read.csv(file.path(cfg$reference_output,"secondary.csv"),stringsAsFactors=FALSE)
    stopifnot(isTRUE(all.equal(s$p_Holm_four_planned,p.adjust(s$p,"holm",n=4L))))
    verify_published_reference(s, cfg$published_secondary, cfg$reference_model)
    reference_Holm<-s$p_Holm_four_planned[s$model==cfg$reference_model]
  }
  details<-list(config=cfg,code=core$fingerprint(h3_source_paths(base,cfg)),
    raw=core$fingerprint(if(synthetic)character()else names(cfg$raw_sha256)),runtime=cfg$runtime,R_version=R.version.string,
    platform=R.version$platform,analysis_type=if(synthetic)"SYNTHETIC_VALIDATION_ONLY"else"REAL_DATA_USER_EXECUTION",
    dataset_sha256=sha_object(dat$sets),plan=h3_plan(),baseline=baseline,
    reference_coefficients_sha256=sha_object(reference$coefficients),reference_Holm=reference_Holm)
  list(base=base,cfg=cfg,sets=dat$sets,flow=dat$flow,sizes=dat$sizes,reference=reference,
    plan=h3_plan(),details=details,signature=sha_object(details))
}
h3_provenance <- function(ctx) {
  core$assert_provenance(ctx)
  if(length(ctx$details$baseline))stopifnot(core$same_manifest(ctx$details$baseline,
    core$fingerprint(c(core$manifest_paths(ctx$cfg$reference_output),file.path(ctx$cfg$reference_output,"manifest.tsv")))))
  invisible(TRUE)
}

verify_published_reference <- function(local, path, model) {
  published <- read.csv(path)
  columns <- c("log_OR", "SE", "OR", "lower95", "upper95", "p", "p_Holm_four_planned")
  target <- local[local$model == model, columns, drop = FALSE]
  expected <- published[published$model == model, columns, drop = FALSE]
  if (nrow(target) != 1L || nrow(expected) != 1L ||
      !all(is.finite(unlist(target))) || !all(is.finite(unlist(expected))) ||
      !all(vapply(columns, function(column) isTRUE(all.equal(
        target[[column]], expected[[column]], tolerance = 1e-5)), logical(1))))
    stop("Locally verified H3 reference differs from the published aggregate reference.")
  invisible(TRUE)
}

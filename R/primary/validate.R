synthetic_data <- function(seed) {
  existed<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(existed)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(existed)assign(".Random.seed",old,.GlobalEnv)else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed);H<-260L;n<-2L*H;hh<-rep(seq_len(H),each=2);reason<-sample(rep(c("1","2"),length.out=H))
  mage<-sample(37:65,H,replace=TRUE);age<-sample(15:29,n,replace=TRUE);sex<-sample(c("1","2"),n,replace=TRUE)
  edu<-sample(c("1","2","31","41","512"),H,replace=TRUE)
  stage<-sample(c("2","31","41","51"),H,replace=TRUE);edu[1]<-"511";stage[1]<-"53"
  region<-sample(c(paste0("TR",1:9),"TRA","TRB","TRC"),H,replace=TRUE)
  household_effect<-rnorm(H,sd=.3)
  eta<-.25*(reason[hh]=="2")-.2*(sex=="2")+.03*(age-22)+.02*(mage[hh]-48)+household_effect[hh]
  ordinal<-function(x)as.character(findInterval(x+qlogis(runif(length(x))),c(-Inf,-1.4,-.3,.5,1.4,Inf)))
  mother_att<-ordinal(.2*(reason=="2")+household_effect)
  d<-data.frame(BIRIMNO=sprintf("synthetic%04d",hh),FERTNO1=as.character(rep(2:3,H)),m_FERTNO1="1",
    CINSIYET1=sex,YAS_BITIRILEN1=as.character(age),m_YAS_BITIRILEN1=as.character(mage[hh]),
    m_EGITIM_YARIM_NEDEN=reason[hh],m_OKUL_BITEN=edu[hh],m_EGITIM_YARIM_SEVIYE=stage[hh],
    m_ANNE_EGITIM=sample(c("1","2"),H,replace=TRUE)[hh],m_BABA_EGITIM=sample(c("1","2","4"),H,replace=TRUE)[hh],
    IBBS_1=region[hh],FAKTOR_FERT=as.character(runif(n,.6,2)),
    ANNE_DOGUM_YIL=as.character(2021-mage[hh]),m_DOGUM_TARIH_YIL=as.character(2021-mage[hh]),
    KADIN_EVISLERI=ordinal(eta),m_KADIN_EVISLERI=mother_att[hh],KADIN_IS=ordinal(-eta),ERKEK_GECIM=ordinal(eta+.2),stringsAsFactors=FALSE)
  flagged<-seq(17,n,by=17);d$ANNE_DOGUM_YIL[flagged]<-as.character(as.numeric(d$ANNE_DOGUM_YIL[flagged])+3)
  d<-build_fields(d);sets<-data_sets(d)
  list(sets=sets,flow=data.frame(stage="SYNTHETIC_ONLY",n=n),sizes=sample_table(sets))
}
expect_failure <- function(fun,label) {
  failed<-inherits(tryCatch({fun();NULL},error=function(e)e),"error")
  if(!failed)stop("Expected rejection did not occur: ",label)
  cat("PASS negative check:",label,"\n")
}
validate_workflow <- function(base,cfg,out) {
  started<-Sys.time();stopifnot(!dir.exists(out))
  m<-fingerprint(source_paths(base));stopifnot(same_manifest(m,m[rev(seq_len(nrow(m))),]))
  changed<-m;changed$bytes[1]<-changed$bytes[1]+1;stopifnot(!same_manifest(m,changed))
  changed<-m;changed$sha256[1]<-paste(rep("0",64),collapse="");stopifnot(!same_manifest(m,changed))
  stopifnot(!same_manifest(m,m[-1,]),!same_manifest(m,rbind(m,m[1,])))
  check_freeze(base)
  cat("PASS manifest row-order independence; changed size/hash, missing and duplicate files rejected.\n")
  expect_failure(function()map_checked("99",c("1"="none"),"fixture"),"unknown code stops instead of silent recoding")
  xx<-cbind(1,c(rep(0,20),rep(1,20)));sep<-separation_check(xx,as.integer(xx[,2]))
  stopifnot(sep$status==0L,isTRUE(sep$separated));cat("PASS separation fixture.\n")
  ctx<-context(base,cfg,TRUE)
  partial<-execute_run(out,ctx,TRUE,stop_after=4L)
  stopifnot(partial$saved==4L,length(all_records(out,ctx))==4L)
  first_hash<-sha_file(file.path(out,"private","packs","pack01.rds"))
  expect_failure(function()initialize_run(out,ctx),"start refuses an existing run")
  changed<-ctx;changed$signature<-paste0("wrong_",ctx$signature)
  expect_failure(function()check_metadata(out,changed),"changed scientific signature refuses resume")
  result<-execute_run(out,ctx,FALSE)
  stopifnot(identical(first_hash,sha_file(file.path(out,"private","packs","pack01.rds"))))
  if(!isTRUE(result$all_models_valid)) {
    print(read.csv(file.path(out,"model_status.csv")),row.names=FALSE)
    stop("Synthetic numerical gate failure; production is not validated")
  }
  verify_run(out,ctx,TRUE)
  task<-as.list(ctx$plan[4,,drop=FALSE]);records<-all_records(out,ctx)
  rescaled<-ctx$sets;rescaled$primary$weight<-7*rescaled$primary$weight
  chk<-fit_task(task,rescaled,cfg);stopifnot(chk$status=="valid")
  original<-records[[4]]$coefficients
  stopifnot(max(abs(original$log_OR-chk$coefficients$log_OR))<1e-4,
    max(abs(original$SE-chk$coefficients$SE))<1e-4)
  check_inputs(cfg,synthetic=TRUE)
  cat("PASS synthetic weight-scale invariance, four-pack checkpoint/resume, 16 model gates, table replay and manifests.\n")
  cat("SYNTHETIC VALIDATION OUTPUT:",out,"\n")
  cat("Validation elapsed seconds:",round(as.numeric(difftime(Sys.time(),started,units="secs")),2),"\n")
  invisible(out)
}

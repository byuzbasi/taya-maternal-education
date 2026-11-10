#!/usr/bin/env Rscript
base <- "R/primary"
suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu="fail",contrasts=c("contr.treatment","contr.poly"))
for(f in c("data.R","models.R","workflow.R","validate.R"))source(file.path(base,f),local=.GlobalEnv)
cfg<-jsonlite::read_json(file.path(base,"config.json"),simplifyVector=TRUE)
args<-commandArgs(trailingOnly=TRUE)[-1L];mode<-if(length(args))args[1]else"help"
if(mode=="help") {
  cat("From the repository root: Rscript --vanilla run.R primary MODE\n",
    "Modes: preflight, validate [new-validation-output], start, resume, status, verify, report, unlock\n",
    "start/resume fit REAL models. validate fits only labeled SYNTHETIC data.\n")
}else if(mode=="preflight") {
  check_freeze(base);ctx<-context(base,cfg);print(ctx$sizes,row.names=FALSE)
  for(i in 1:nrow(ctx$plan)) {
    task<-as.list(ctx$plan[i,,drop=FALSE]);z<-estimable_design(task,droplevels(ctx$sets[[task$sample]]))
    X<-model.matrix(z$formula,z$data);stopifnot(qr(X)$rank==ncol(X),all(complete.cases(model.frame(z$formula,z$data,na.action=na.pass))))
    if(length(z$alias))cat("EQUIVALENT nuisance representation:",task$id,"; recorded alias:",z$alias,"\n")
  }
  cat("PASS real-data preflight: all 16 design matrices full rank; no model fitted.\n")
}else if(mode=="validate") {
  out<-if(length(args)>=2)args[2]else paste0(cfg$output,"_validation_",format(Sys.time(),"%Y%m%d_%H%M%S"))
  validate_workflow(base,cfg,out)
}else if(mode=="status") {
  p<-file.path(cfg$output,"progress.json");if(!file.exists(p))stop("No production progress exists")
  cat(readLines(p,warn=FALSE),sep="\n")
}else if(mode=="unlock") {
  unlock_writer(cfg$output)
}else if(mode %in% c("start","resume","verify","report")) {
  check_freeze(base);ctx<-context(base,cfg)
  if(mode %in% c("start","resume")) {
    result<-execute_run(cfg$output,ctx,start=mode=="start")
    verify_expected_primary(cfg$output,result)
  }else if(mode=="verify")verify_expected_primary(cfg$output,verify_run(cfg$output,ctx,strict=FALSE))
  else {verify_run(cfg$output,ctx,strict=FALSE);cat(readLines(file.path(cfg$output,"SONUCLAR.txt"),warn=FALSE),sep="\n")}
}else stop("Unknown mode: ",mode)

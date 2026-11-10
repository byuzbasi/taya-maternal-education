#!/usr/bin/env Rscript
base <- "R/h3"
suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu="fail",contrasts=c("contr.treatment","contr.poly"))
for(f in c("setup.R","summaries.R","workflow.R","validate.R"))source(file.path(base,f))
cfg<-jsonlite::read_json(file.path(base,"config.json"),simplifyVector=TRUE)
args<-commandArgs(trailingOnly=TRUE)[-1L];mode<-if(length(args))args[1]else"help"
if(mode=="help") {
  cat("Modes: preflight, validate [new-output], validate_verify OUTPUT, start, resume, status, unlock, verify, report\n",
    "start/resume fit four NEW REAL models, only in the user's terminal. validate fits SYNTHETIC data only.\n")
}else if(mode=="status") {
  p<-file.path(cfg$output,"progress.json");if(!file.exists(p))stop("No H3 production progress exists")
  cat(readLines(p,warn=FALSE),sep="\n")
}else if(mode=="unlock") {
  core$unlock_writer(cfg$output)
}else {
  h3_freeze(base,cfg)
  if(mode=="validate") {
    out<-if(length(args)>=2)args[2]else paste0(cfg$output,"_validation_",format(Sys.time(),"%Y%m%d_%H%M%S"))
    h3_validation(base,cfg,out)
  }else if(mode=="validate_verify") {
    stopifnot(length(args)==2L);ctx<-h3_reload_synthetic(base,cfg,args[2]);h3_verify(args[2],ctx)
  }else if(mode %in% c("preflight","start","resume","verify","report")) {
    ctx<-h3_context(base,cfg)
    if(mode=="preflight")h3_preflight(ctx)
    else if(mode %in% c("start","resume")) {
      s<-h3_execute(cfg$output,ctx,start=mode=="start")
      if(!isTRUE(s$all_new_models_valid))quit(status=2L)
    }else if(mode=="verify")h3_verify(cfg$output,ctx)
    else {h3_verify(cfg$output,ctx,FALSE);cat(readLines(file.path(cfg$output,"SONUCLAR.txt"),warn=FALSE),sep="\n")}
  }else stop("Unknown mode: ",mode)
}

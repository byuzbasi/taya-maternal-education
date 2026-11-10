model_plan <- function() {
  m0 <- "reason"
  m1 <- paste(m0,"+ child_age + sex + mother_age + gm_school + gf_school")
  m2 <- paste(m1,"+ mother_education + interruption");m3<-paste(m2,"+ region")
  out<-data.frame(id=c("M0","M1","M2","M3","H2_less_than_mother","H3_reason_by_sex","secondary_woman_job","secondary_man_provider",
    "sensitivity_age15_29","sensitivity_shared_education","sensitivity_link_flags","sensitivity_unweighted",paste0("threshold_gt",1:4)),
    sample=c(rep("primary",8),"age15_29","shared","links",rep("primary",5)),
    kind=c(rep("ordinal",4),"binary","ordinal","ordinal","ordinal",rep("ordinal",4),rep("binary",4)),
    outcome=c(rep("KADIN_EVISLERI",4),"less_traditional","KADIN_EVISLERI","KADIN_IS","ERKEK_GECIM",rep("KADIN_EVISLERI",4),paste0("gt",1:4)),
    rhs=c(m0,m1,m2,m3,m3,paste(m3,"+ reason:sex"),rep(m3,10)),
    weighted=c(rep(TRUE,11),FALSE,rep(TRUE,4)),stringsAsFactors=FALSE)
  out$pack<-rep(1:4,each=4);out$index<-seq_len(nrow(out));out
}
make_design <- function(d,weighted=TRUE) {
  d<-droplevels(d);d$analysis_weight<-if(weighted)d$weight else rep(1,nrow(d))
  survey::svydesign(ids=as.formula("~household",env=baseenv()),
    weights=as.formula("~analysis_weight",env=baseenv()),data=d)
}
model_formula <- function(task) as.formula(paste(task$outcome,"~",task$rhs),env=baseenv())
estimable_design <- function(task,d) {
  f<-model_formula(task);X<-model.matrix(f,d);q<-qr(X);alias<-character()
  if(q$rank<ncol(X)) {
    alias<-colnames(X)[q$pivot[seq.int(q$rank+1L,ncol(X))]]
    # Only this preflight-observed nuisance identity is authorized here.
    stopifnot(task$id=="sensitivity_shared_education",identical(alias,"interruptionhigher"),
      all(X[,"interruptionhigher"]==X[,"mother_educationhigh"]+X[,"mother_educationhigher"]))
    keep<-setdiff(colnames(X),alias);stopifnot("reasonfamily" %in% keep)
    nuisance<-setdiff(keep,c("(Intercept)","reasonfamily"))
    for(n in nuisance)d[[n]]<-X[,n]
    f<-reformulate(c("reason",nuisance),response=task$outcome,env=baseenv())
    Z<-model.matrix(f,d)
    stopifnot(qr(Z)$rank==ncol(Z),qr(cbind(Z,X))$rank==ncol(Z),identical(colnames(Z),keep))
  }
  list(data=d,formula=f,alias=alias,original_columns=ncol(X),original_rank=q$rank)
}
coefficient_table <- function(fit,task,d) {
  b<-if(task$kind=="ordinal")coef(fit,intercepts=FALSE)else coef(fit)
  V<-vcov(fit)[names(b),names(b),drop=FALSE];se<-sqrt(diag(V));df<-fit$df.residual
  crit<-qt(.975,df);pv<-2*pt(-abs(b/se),df)
  data.frame(model=task$id,sample=task$sample,outcome=task$outcome,kind=task$kind,
    term=names(b),log_OR=unname(b),SE=unname(se),OR=unname(exp(b)),
    lower95=unname(exp(b-crit*se)),upper95=unname(exp(b+crit*se)),p=unname(pv),df=df,
    pairs=nrow(d),mothers=length(unique(d$mother_id)),households=length(unique(d$household)),weighted=task$weighted,row.names=NULL)
}
ordinal_scores <- function(fit) {
  d<-fit$survey.design$variables;X<-model.matrix(delete.response(fit$terms),d,contrasts.arg=fit$contrasts)
  X<-X[,names(fit$coefficients),drop=FALSE]; y<-as.integer(d[[all.vars(fit$terms)[1]]]);eta<-drop(X%*%fit$coefficients)
  z<-c(-Inf,fit$zeta,Inf);lo<-z[y]-eta;hi<-z[y+1L]-eta
  pr<-plogis(hi)-plogis(lo);fl<-dlogis(lo);fh<-dlogis(hi)
  G<-X*((fl-fh)/pr)
  Z<-vapply(seq_along(fit$zeta),function(j)((y==j)*fh-(y==j+1L)*fl)/pr,numeric(length(y)))
  colnames(Z)<-names(fit$zeta);G<-cbind(G,Z);colnames(G)<-names(coef(fit))
  G
}
audit_fit <- function(fit,task,d,score_tolerance) {
  X<-model.matrix(fit$terms,fit$survey.design$variables);V<-vcov(fit)
  stopifnot(qr(X)$rank==ncol(X),all(is.finite(coef(fit))),all(is.finite(V)),fit$df.residual>0)
  ve<-eigen((V+t(V))/2,symmetric=TRUE,only.values=TRUE)$values;stopifnot(min(ve)>0)
  a<-list(model=task$id,kind=task$kind,pairs=nrow(d),mothers=length(unique(d$mother_id)),
    households=length(unique(d$household)),design_rank=qr(X)$rank,design_columns=ncol(X),
    df=fit$df.residual,covariance_min_eigenvalue=min(ve),warnings=character())
  if(task$kind=="ordinal") {
    stopifnot(identical(as.integer(fit$convergence),0L),all(diff(fit$zeta)>0),nrow(fit$survey.design$variables)==nrow(d))
    pr<-as.matrix(predict(fit,type="probs"));stopifnot(all(is.finite(pr)),all(pr>0),all(pr<1),max(abs(rowSums(pr)-1))<1e-8)
    he<-eigen((fit$Hessian+t(fit$Hessian))/2,symmetric=TRUE,only.values=TRUE)$values;stopifnot(min(he)>0)
    G<-ordinal_scores(fit);w<-as.numeric(weights(fit$survey.design,"sampling"));S<-G*w
    rms<-sqrt(colSums(G^2*w)/sum(w));score<-max(abs(colSums(S)/sum(w))/pmax(1,rms))
    stopifnot(is.finite(score),score<=score_tolerance)
    influence<-S%*%solve(fit$Hessian);cs<-rowsum(influence,d$household,reorder=FALSE)
    a$converged<-TRUE;a$optimizer_counts<-fit$niter;a$scaled_score<-score
    a$hessian_min_eigenvalue<-min(he);a$hessian_condition<-max(he)/min(he)
    a$thresholds<-fit$zeta;a$probability_min<-min(pr);a$probability_max<-max(pr)
    a$max_household_reason_influence_in_SE<-max(abs(cs[,"reasonfamily"]))/sqrt(V["reasonfamily","reasonfamily"])
  }else {
    stopifnot(isTRUE(fit$converged),nrow(fit$survey.design$variables)==nrow(d),all(is.finite(fitted(fit))))
    a$converged<-fit$converged;a$iterations<-fit$iter;a$fitted_min<-min(fitted(fit));a$fitted_max<-max(fitted(fit))
  };a
}
fit_task <- function(task,sets,cfg) {
  started<-Sys.time();warns<-character();sep<-NULL
  result<-tryCatch({
    setup<-estimable_design(task,droplevels(sets[[task$sample]]));d<-setup$data;design<-make_design(d,task$weighted)
    f<-setup$formula;X<-model.matrix(f,d);stopifnot(qr(X)$rank==ncol(X),all(complete.cases(model.frame(f,d,na.action=na.pass))))
    objective_scale<-mean(as.numeric(weights(design,"sampling")))
    if(task$kind=="binary") {
      y<-model.response(model.frame(f,d));stopifnot(identical(sort(unique(y)),0:1))
      sep<-separation_check(X,y);if(sep$status!=0L || is.na(sep$separated) || sep$separated)stop("Binary separation/feasibility gate rejected this fixed model")
    }
    fit<-withCallingHandlers(if(task$kind=="ordinal") {
      survey::svyolr(f,design=design,method="logistic",na.action=na.fail,
        control=list(maxit=cfg$optimizer_maxit,reltol=cfg$optimizer_reltol,fnscale=objective_scale))
    }else survey::svyglm(f,design=design,family=quasibinomial(),rescale=TRUE,na.action=na.fail,influence=TRUE),
      warning=function(w){warns<<-c(warns,conditionMessage(w));invokeRestart("muffleWarning")})
    audit<-audit_fit(fit,task,d,cfg$scaled_score_tolerance);audit$warnings<-warns;audit$separation<-sep
    audit$nuisance_alias<-setup$alias;audit$original_design_columns<-setup$original_columns;audit$original_design_rank<-setup$original_rank
    table<-coefficient_table(fit,task,d)
    list(status=if(length(warns))"rejected"else"valid",error=if(length(warns))paste(warns,collapse="; ")else NULL,fit=fit,audit=audit,coefficients=table)
  },error=function(e)list(status="rejected",error=conditionMessage(e),fit=NULL,audit=list(warnings=warns,separation=sep),coefficients=NULL))
  result$id<-task$id;result$sample<-task$sample;result$elapsed_seconds<-as.numeric(difftime(Sys.time(),started,units="secs"));result
}
validate_record <- function(r,task,sets,cfg) {
  stopifnot(identical(r$id,task$id),identical(r$sample,task$sample),r$status %in% c("valid","rejected"),is.finite(r$elapsed_seconds),r$elapsed_seconds>=0)
  if(r$status=="valid") {
    stopifnot(!is.null(r$fit),is.null(r$error),!length(r$audit$warnings))
    setup<-estimable_design(task,droplevels(sets[[task$sample]]))
    stopifnot(identical(r$fit$survey.design$variables[,names(setup$data),drop=FALSE],setup$data),identical(r$audit$nuisance_alias,setup$alias))
    a<-audit_fit(r$fit,task,setup$data,cfg$scaled_score_tolerance)
    stopifnot(a$pairs==r$audit$pairs,a$households==r$audit$households,
      isTRUE(all.equal(r$coefficients,coefficient_table(r$fit,task,droplevels(sets[[task$sample]])),tolerance=1e-12)))
  }else stopifnot(is.character(r$error),length(r$error)==1L,nzchar(r$error))
  invisible(TRUE)
}
observed_table <- function(d) {
  do.call(rbind,lapply(c("KADIN_EVISLERI","KADIN_IS","ERKEK_GECIM"),function(v)
    do.call(rbind,lapply(levels(d$reason),function(g) {
      z<-d[d$reason==g,,drop=FALSE];data.frame(outcome=v,reason=g,category=1:5,
        n=tabulate(as.integer(z[[v]]),5),weighted_percent=vapply(1:5,function(k)100*sum(z$weight[as.integer(z[[v]])==k])/sum(z$weight),0))
    }))))
}

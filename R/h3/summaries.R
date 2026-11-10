h3_contrasts <- function(fit,task,d) {
  b<-coef(fit,intercepts=FALSE);V<-vcov(fit)[names(b),names(b),drop=FALSE]
  do.call(rbind,lapply(c("sons","daughters","daughter_son_ratio_of_ORs"),function(label) {
    L<-setNames(rep(0,length(b)),names(b))
    if(label!="daughter_son_ratio_of_ORs")L["reasonfamily"]<-1
    if(label!="sons")L["reasonfamily:sexgirl"]<-1
    est<-sum(L*b);se<-sqrt(drop(t(L)%*%V%*%L));df<-fit$df.residual;crit<-qt(.975,df)
    data.frame(model=task$id,sample=task$sample,contrast=label,log_OR=est,SE=se,
      OR=exp(est),lower95=exp(est-crit*se),upper95=exp(est+crit*se),
      p_unadjusted=2*pt(-abs(est/se),df),df=df,pairs=nrow(d),
      mothers=length(unique(d$mother_id)),households=length(unique(d$household)),weighted=task$weighted)
  }))
}
h3_probabilities <- function(fit,task,d) {
  setup<-h3_design(task,d);w<-if(task$weighted)d$weight else rep(1,nrow(d))
  do.call(rbind,lapply(levels(d$reason),function(g)
    do.call(rbind,lapply(levels(d$sex),function(s) {
      z<-setup$data;z$reason<-factor(rep(g,nrow(z)),levels(d$reason))
      z$sex<-factor(rep(s,nrow(z)),levels(d$sex))
      pr<-as.matrix(predict(fit,newdata=z,type="probs"))
      stopifnot(identical(dim(pr),c(nrow(d),5L)),all(is.finite(pr)),
        all(pr>0 & pr<1),max(abs(rowSums(pr)-1))<1e-8)
      value<-colSums(pr*w)/sum(w);stopifnot(abs(sum(value)-1)<1e-10)
      data.frame(model=task$id,sample=task$sample,reason=g,sex=s,category=1:5,
        probability=unname(value),weighted_reference=task$weighted,
        reference="Common pooled covariate distribution within this model sample",
        uncertainty="Descriptive point estimates; no probability tests or probability intervals")
    }))))
}
h3_influence <- function(fit,task,d) {
  G<-core$ordinal_scores(fit);w<-as.numeric(weights(fit$survey.design,"sampling"))
  I<-(G*w)%*%solve(fit$Hessian)
  sv<-survey::svyrecvar(I,fit$survey.design$cluster,fit$survey.design$strata,
    fit$survey.design$fpc,postStrata=fit$survey.design$postStrata)
  stopifnot(isTRUE(all.equal(unname(sv),unname(vcov(fit)),tolerance=1e-7)))
  hh<-rowsum(I,d$household,reorder=FALSE);term<-"reasonfamily:sexgirl"
  score<-abs(hh[,term])/sqrt(vcov(fit)[term,term])
  data.frame(model=task$id,sample=task$sample,
    max_abs_household_interaction_in_SE=max(score),median_abs_household_interaction_in_SE=median(score),
    p90_abs_household_interaction_in_SE=unname(quantile(score,.9)),
    max_share_of_uncentered_squared_contributions=max(score^2)/sum(score^2),
    survey_covariance_reproduced=TRUE,diagnostic="Linearized contributions; no deletion refits or automatic exclusions")
}
h3_details <- function(r,task,sets) {
  d<-droplevels(sets[[task$sample]])
  list(contrasts=h3_contrasts(r$fit,task,d),probabilities=h3_probabilities(r$fit,task,d),
    influence=h3_influence(r$fit,task,d))
}
h3_validate <- function(r,task,sets,cfg) {
  original_validation(r,task,sets,cfg)
  if(task$id %in% h3_plan()$id && r$status=="valid")
    stopifnot(isTRUE(all.equal(r$h3,h3_details(r,task,sets),tolerance=1e-12)))
  invisible(TRUE)
}
core$validate_record <- h3_validate
h3_fit <- function(task,sets,cfg) {
  r<-core$fit_task(task,sets,cfg)
  if(r$status=="valid") {
    extra<-tryCatch(h3_details(r,task,sets),error=function(e)e)
    if(inherits(extra,"error")){r$status<-"rejected";r$error<-paste("H3 reporting gate:",conditionMessage(extra))}
    else r$h3<-extra
  };r
}
h3_support <- function(ctx) {
  do.call(rbind,lapply(seq_len(nrow(ctx$plan)),function(i) {
    task<-as.list(ctx$plan[i,,drop=FALSE]);d<-ctx$sets[[task$sample]]
    do.call(rbind,lapply(levels(d$reason),function(g)
      do.call(rbind,lapply(levels(d$sex),function(s) {
        z<-d[d$reason==g & d$sex==s,,drop=FALSE];w<-if(task$weighted)z$weight else rep(1,nrow(z))
        data.frame(model=task$id,sample=task$sample,reason=g,sex=s,
          pairs=nrow(z),mothers=length(unique(z$mother_id)),households=length(unique(z$household)),
          category=1:5,n=tabulate(as.integer(z$KADIN_EVISLERI),5),
          analysis_weighted=task$weighted,
          analysis_percent=vapply(1:5,function(k)100*sum(w[as.integer(z$KADIN_EVISLERI)==k])/sum(w),0))
      }))))
  }))
}
h3_covariates <- function(ctx) do.call(rbind,lapply(seq_len(nrow(ctx$plan)),function(i) {
  task<-as.list(ctx$plan[i,,drop=FALSE]);d<-ctx$sets[[task$sample]]
  do.call(rbind,lapply(levels(d$sex),function(s) {
    z<-core$comparison_table(d[d$sex==s,,drop=FALSE]);z<-z[z$variable!="sex",,drop=FALSE]
    data.frame(model=task$id,sex=s,z)
  }))
}))
h3_education_support <- function(ctx) do.call(rbind,lapply(seq_len(nrow(ctx$plan)),function(i) {
  task<-as.list(ctx$plan[i,,drop=FALSE]);d<-ctx$sets[[task$sample]]
  d<-d[!duplicated(paste(d$mother_id,d$sex)),,drop=FALSE]
  tab<-as.data.frame(table(completed_education=factor(d$m_OKUL_BITEN,
    sort(unique(d$m_OKUL_BITEN),method="radix")),interrupted_stage=factor(d$m_EGITIM_YARIM_SEVIYE,
    sort(unique(d$m_EGITIM_YARIM_SEVIYE),method="radix")),reason=d$reason,sex=d$sex),stringsAsFactors=FALSE)
  names(tab)[5]<-"mothers_within_child_sex";data.frame(model=task$id,tab)
}))
h3_tables <- function(records,ctx) {
  rp<-core$model_plan();rt<-as.list(rp[rp$id==ctx$cfg$reference_model,,drop=FALSE])
  reference<-ctx$reference;reference$h3<-h3_details(reference,rt,ctx$sets)
  valid<-c(list(reference),Filter(function(r)r$status=="valid",records))
  bind<-function(n)do.call(rbind,lapply(valid,function(r)r$h3[[n]]))
  contrasts<-bind("contrasts");interaction<-contrasts[contrasts$contrast=="daughter_son_ratio_of_ORs",,drop=FALSE]
  interaction$p_Holm_original_four<-ifelse(interaction$model==rt$id,ctx$details$reference_Holm,NA_real_)
  interaction$p_Holm_new_four<-NA_real_;new<-interaction$model!=rt$id
  interaction$p_Holm_new_four[new]<-p.adjust(interaction$p_unadjusted[new],"holm",n=4L)
  status<-do.call(rbind,lapply(records,function(r)data.frame(model=r$id,sample=r$sample,
    status=r$status,error=if(is.null(r$error))NA_character_ else r$error,elapsed_seconds=r$elapsed_seconds)))
  list(model_status=status,contrasts=contrasts,interactions=interaction,
    probabilities=bind("probabilities"),influence=bind("influence"),support=h3_support(ctx),
    covariates=h3_covariates(ctx),education_support=h3_education_support(ctx))
}

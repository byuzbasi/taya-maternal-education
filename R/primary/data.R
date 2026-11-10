read_fields <- function(path, columns) {
  header <- names(read.csv(path, sep=";", nrows=0, fileEncoding="UTF-8-BOM", check.names=FALSE))
  stopifnot(all(columns %in% header))
  classes <- ifelse(header %in% columns, "character", "NULL")
  read.csv(path, sep=";", colClasses=classes, fileEncoding="UTF-8-BOM",
           check.names=FALSE, na.strings=character(), stringsAsFactors=FALSE)
}
person_key <- function(household, person) paste(household, person, sep=":")
map_checked <- function(x, mapping, label) {
  z <- unname(mapping[x]); if(anyNA(z)) stop("Unspecified code in ",label,": ",paste(unique(x[is.na(z)]),collapse=","))
  z
}
build_fields <- function(d) {
  d$reason <- factor(ifelse(d$m_EGITIM_YARIM_NEDEN=="1","economic","family"), c("economic","family"))
  d$sex <- factor(map_checked(d$CINSIYET1,c("1"="boy","2"="girl"),"sex"),c("boy","girl"))
  d$child_age <- as.numeric(d$YAS_BITIRILEN1); d$mother_age <- as.numeric(d$m_YAS_BITIRILEN1)
  education <- c("1"="none","2"="primary","31"="lower","32"="lower","33"="lower",
    "41"="high","42"="high","511"="higher","512"="higher","521"="higher","522"="higher","53"="higher")
  interruption <- c("2"="primary","31"="lower","32"="lower","33"="lower","41"="high","42"="high",
    "51"="higher","52"="higher","53"="higher")
  d$mother_education <- factor(map_checked(d$m_OKUL_BITEN,education,"mother education"),c("none","primary","lower","high","higher"))
  d$interruption <- factor(map_checked(d$m_EGITIM_YARIM_SEVIYE,interruption,"interruption"),c("primary","lower","high","higher"))
  parent_map <- c("1"="none","2"="at_least_primary","3"="at_least_primary","4"="at_least_primary","5"="at_least_primary")
  d$gm_school <- factor(map_checked(d$m_ANNE_EGITIM,parent_map,"grandmother"),c("none","at_least_primary"))
  d$gf_school <- factor(map_checked(d$m_BABA_EGITIM,parent_map,"grandfather"),c("none","at_least_primary"))
  regions <- c(paste0("TR",1:9),"TRA","TRB","TRC")
  stopifnot(all(d$IBBS_1 %in% regions)); d$region <- factor(d$IBBS_1,regions)
  for(v in c("KADIN_EVISLERI","KADIN_IS","ERKEK_GECIM","m_KADIN_EVISLERI")) {
    stopifnot(all(d[[v]] %in% as.character(1:5))); d[[v]] <- ordered(d[[v]],levels=as.character(1:5))
  }
  d$less_traditional <- as.integer(as.integer(d$KADIN_EVISLERI)<as.integer(d$m_KADIN_EVISLERI))
  for(k in 1:4) d[[paste0("gt",k)]] <- as.integer(as.integer(d$KADIN_EVISLERI)>k)
  d$weight <- as.numeric(sub(",",".",d$FAKTOR_FERT,fixed=TRUE))
  d$household <- d$BIRIMNO; d$mother_id <- person_key(d$BIRIMNO,d$m_FERTNO1)
  reported <- suppressWarnings(as.numeric(d$ANNE_DOGUM_YIL)); roster <- suppressWarnings(as.numeric(d$m_DOGUM_TARIH_YIL))
  known <- !is.na(reported)&!is.na(roster)&reported>=1850&reported<=2006&roster>=1850&roster<=2006
  d$birthyear_unknown <- !known
  d$link_flag <- (d$mother_age-d$child_age<15) | (known & abs(reported-roster)>1)
  stopifnot(all(is.finite(d$weight)),all(d$weight>0),all(is.finite(d$child_age)),all(is.finite(d$mother_age)),!anyNA(d$link_flag))
  d
}
data_sets <- function(d) {
  p <- d[d$child_age>=18 & d$child_age<=29,,drop=FALSE]
  s <- d[d$child_age>=15 & d$child_age<=29,,drop=FALSE]
  u <- p[!duplicated(p$mother_id),,drop=FALSE]
  cell <- paste(u$m_OKUL_BITEN,u$m_EGITIM_YARIM_SEVIYE,sep=":")
  both <- names(which(tapply(as.character(u$reason),cell,function(z)length(unique(z)))==2L))
  shared <- paste(p$m_OKUL_BITEN,p$m_EGITIM_YARIM_SEVIYE,sep=":") %in% both
  list(primary=p,age15_29=s,shared=p[shared,,drop=FALSE],links=p[!p$link_flag,,drop=FALSE])
}
sample_table <- function(sets) do.call(rbind,lapply(names(sets),function(name) {
  d <- sets[[name]];do.call(rbind,lapply(levels(d$reason),function(g) {
    z <- d[d$reason==g,,drop=FALSE]
    data.frame(sample=name,reason=g,pairs=nrow(z),mothers=length(unique(z$mother_id)),
      households=length(unique(z$household)),link_flags=sum(z$link_flag),unknown_birthyears=sum(z$birthyear_unknown))
  }))
}))
load_real_data <- function(cfg) {
  fcols <- c("BIRIMNO","FERTNO1","CINSIYET1","YAS_BITIRILEN1","EGITIM_YARIM","EGITIM_YARIM_SEVIYE",
    "EGITIM_YARIM_NEDEN","ANNE_EGITIM","BABA_EGITIM","ANNE_DOGUM_YIL","OZ_COCUK","FAKTOR_FERT","IBBS_1",
    "KADIN_EVISLERI","KADIN_IS","ERKEK_GECIM")
  rcols <- c("BIRIMNO","FERTNO","CINSIYET","YAS_BITIRILEN","FERTNO_ANNE","ADRES_IKAMET_DURUM","OKUL_BITEN","DOGUM_TARIH_YIL")
  f <- read_fields("data/microdata/TAYA2021_Fert_MikroVeri.csv",fcols)
  r <- read_fields("data/microdata/TAYA2021_Fertliste_MikroVeri.csv",rcols)
  h <- read_fields("data/microdata/TAYA2021_Hane_MikroVeri.csv","BIRIMNO")
  fk <- person_key(f$BIRIMNO,f$FERTNO1);rk <- person_key(r$BIRIMNO,r$FERTNO)
  stopifnot(nrow(f)==42043L,nrow(r)==60967L,nrow(h)==19428L,!anyDuplicated(fk),!anyDuplicated(rk),!anyDuplicated(h$BIRIMNO),all(f$BIRIMNO %in% h$BIRIMNO))
  ii <- match(fk,rk); stopifnot(!anyNA(ii),all(f$CINSIYET1==r$CINSIYET[ii]),all(f$YAS_BITIRILEN1==r$YAS_BITIRILEN[ii]))
  a <- cbind(f,r[ii,setdiff(names(r),c("BIRIMNO","FERTNO","CINSIYET","YAS_BITIRILEN")),drop=FALSE]);row.names(a)<-NULL
  mm <- match(person_key(a$BIRIMNO,a$FERTNO_ANNE),fk); keep <- !is.na(mm)
  mothers <- a[mm[keep],,drop=FALSE];names(mothers)<-paste0("m_",names(mothers))
  d <- cbind(a[keep,,drop=FALSE],mothers);row.names(d)<-NULL
  flow <- data.frame(stage=c("interviewed_people","own_roster_match","interviewed_mother_links"),n=c(nrow(f),nrow(a),nrow(d)))
  d <- d[d$m_CINSIYET1=="2" & d$m_EGITIM_YARIM=="1" & d$m_EGITIM_YARIM_NEDEN %in% c("1","2"),,drop=FALSE]
  flow <- rbind(flow,data.frame(stage="two_reasons_before_residency",n=nrow(d)))
  d <- d[d$ADRES_IKAMET_DURUM=="1" & d$m_ADRES_IKAMET_DURUM=="1",,drop=FALSE]
  # Field mappings are applied only to the approved 15-29 age scope.
  age <- as.numeric(d$YAS_BITIRILEN1);d <- d[age>=15 & age<=29,,drop=FALSE]
  d <- build_fields(d);d<-d[order(d$BIRIMNO,as.numeric(d$FERTNO1)),,drop=FALSE];row.names(d)<-NULL
  sets <- data_sets(d); sizes <- sample_table(sets)
  expected <- list(primary=c(cfg$primary_n,cfg$primary_mothers),age15_29=c(cfg$sensitivity_n,cfg$sensitivity_mothers),
    shared=c(cfg$shared_n,cfg$shared_mothers),links=c(cfg$link_sensitivity_n,cfg$link_sensitivity_mothers))
  for(n in names(sets)) stopifnot(nrow(sets[[n]])==expected[[n]][1],length(unique(sets[[n]]$mother_id))==expected[[n]][2])
  stopifnot(sum(sizes$pairs[sizes$sample=="primary" & sizes$reason=="family"])==191L)
  list(sets=sets,flow=flow,sizes=sizes)
}
comparison_table <- function(d) {
  ans <- list(); add <- function(x) ans[[length(ans)+1L]] <<- x
  for(scope in c("children","mothers")) {
    z <- if(scope=="mothers")d[!duplicated(d$mother_id),,drop=FALSE]else d
    vars <- if(scope=="mothers")c("mother_age","mother_education","interruption","gm_school","gf_school","region")else c("child_age","sex")
    for(v in vars) {
      if(is.numeric(z[[v]])) {
        x<-z[[v]][z$reason=="family"];y<-z[[v]][z$reason=="economic"]
        add(data.frame(scope=scope,variable=v,level="mean",family_n=length(x),economic_n=length(y),family_value=mean(x),economic_value=mean(y),smd=(mean(x)-mean(y))/sqrt((var(x)+var(y))/2)))
      }else for(l in levels(z[[v]])) {
        x<-z[[v]][z$reason=="family"]==l;y<-z[[v]][z$reason=="economic"]==l;p<-mean(x);q<-mean(y);den<-sqrt((p*(1-p)+q*(1-q))/2)
        add(data.frame(scope=scope,variable=v,level=l,family_n=sum(x),economic_n=sum(y),family_value=p,economic_value=q,smd=if(den>0)(p-q)/den else if(p==q)0 else NA_real_))
      }
    }
  };do.call(rbind,ans)
}

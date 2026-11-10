# Produce the two aggregate panels for Supplementary Table S12.
args <- commandArgs(trailingOnly = TRUE)[-1L]
if (length(args) != 1L || dir.exists(args[1]) || file.exists(args[1])) {
  stop("Pass a new output directory from the repository root.")
}
out <- args[1]
source("R/primary/data.R")
source("R/primary/workflow.R")
check_freeze("R/primary")
check_inputs(jsonlite::read_json("R/primary/config.json", simplifyVector = TRUE))
f <- read_fields("data/microdata/TAYA2021_Fert_MikroVeri.csv",
  c("BIRIMNO", "FERTNO1", "YAS_BITIRILEN1", "CINSIYET1", "FAKTOR_FERT",
    "MEDENI_DURUM1", "EVLILIK_SAYI", "EVLILIK_ILK_YAS",
    "KADIN_EVISLERI", "EGITIM_YARIM", "EGITIM_YARIM_NEDEN"))
r <- read_fields("data/microdata/TAYA2021_Fertliste_MikroVeri.csv",
  c("BIRIMNO", "FERTNO", "YAS_BITIRILEN", "CINSIYET", "MEDENI_DURUM",
    "FERTNO_ANNE", "ADRES_IKAMET_DURUM"))
fk <- person_key(f$BIRIMNO, f$FERTNO1)
rk <- person_key(r$BIRIMNO, r$FERTNO)
stopifnot(!anyDuplicated(fk), !anyDuplicated(rk),
  all(grepl("^[0-9]+$", f$YAS_BITIRILEN1)))
y <- which(as.numeric(f$YAS_BITIRILEN1) %in% 18:29)
young <- f[y, , drop = FALSE]
self <- match(fk[y], rk)
stopifnot(!anyNA(self),
  all(young$YAS_BITIRILEN1 == r$YAS_BITIRILEN[self]),
  all(young$CINSIYET1 == r$CINSIYET[self]),
  all(young$MEDENI_DURUM1 == r$MEDENI_DURUM[self]))

pointer <- r$FERTNO_ANNE[self]
mk <- person_key(young$BIRIMNO, pointer)
mother_roster <- match(mk, rk)
mother_fert <- match(mk, fk)
roster_found <- !is.na(mother_roster)
interview_found <- !is.na(mother_fert)
stopifnot(all(!interview_found | roster_found),
  !any(roster_found & pointer == "99"),
  !any(mother_roster[roster_found] == self[roster_found]))
coresident <- roster_found & r$CINSIYET[mother_roster] == "2" &
  r$ADRES_IKAMET_DURUM[self] == "1" & r$ADRES_IKAMET_DURUM[mother_roster] == "1"
available <- coresident & interview_found
unavailable <- coresident & !interview_found
eligible <- available & f$CINSIYET1[mother_fert] == "2" &
  f$EGITIM_YARIM[mother_fert] == "1" & f$EGITIM_YARIM_NEDEN[mother_fert] %in% c("1", "2")
unmatched99 <- !roster_found & pointer == "99"
unmatched_other <- !roster_found & pointer != "99"
other_roster <- roster_found & !coresident
stopifnot(all(rowSums(cbind(coresident, unmatched99, unmatched_other, other_roster)) == 1L),
  identical(as.integer(c(nrow(young), sum(coresident), sum(unmatched99), sum(unmatched_other),
    sum(other_roster), sum(available), sum(unavailable), sum(eligible))),
    c(8604L, 5350L, 2947L, 304L, 3L, 5136L, 214L, 379L)))

# Validate exact pair identities against the approved preparation pipeline.
cfg <- list(primary_n = 379L, primary_mothers = 274L,
  sensitivity_n = 529L, sensitivity_mothers = 366L,
  shared_n = 355L, shared_mothers = 254L, link_sensitivity_n = 362L,
  link_sensitivity_mothers = 261L)
primary <- load_real_data(cfg)$sets$primary
stopifnot(setequal(paste(fk[y[eligible]], mk[eligible]),
  paste(person_key(primary$BIRIMNO, primary$FERTNO1), primary$mother_id)))

n <- nrow(young)
flow <- data.frame(
  status = c("all", "coresident", "unmatched99", "unmatched_other", "other_roster",
    "available", "unavailable", "eligible"),
  young_adults = c(n, sum(coresident), sum(unmatched99), sum(unmatched_other),
    sum(other_roster), sum(available), sum(unavailable), sum(eligible)),
  denominator = c(rep(n, 5), rep(sum(coresident), 2), sum(available)))
flow$percent <- 100 * flow$young_adults / flow$denominator

w <- as.numeric(sub(",", ".", young$FAKTOR_FERT, fixed = TRUE))
age <- as.numeric(young$YAS_BITIRILEN1)
never <- young$MEDENI_DURUM1 == "1"
stopifnot(all(is.finite(w) & w > 0), all(young$CINSIYET1 %in% c("1", "2")),
  all(young$MEDENI_DURUM1 %in% c("1", "21", "22", "23", "24", "25", "3", "4")),
  all(young$EVLILIK_SAYI[never] == ""),
  all(young$EVLILIK_ILK_YAS[never] %in% c("", "98", "99")),
  all(young$KADIN_EVISLERI %in% as.character(1:5)))

profile <- function(g) {
  pct <- function(x) 100 * weighted.mean(x[g], w[g])
  c(sum(g), length(unique(young$BIRIMNO[g])),
    weighted.mean(age[g], w[g]), pct(young$CINSIYET1 == "2"), pct(never),
    vapply(as.character(1:5), function(k) pct(young$KADIN_EVISLERI == k), numeric(1)))
}
profiles <- data.frame(
  characteristic = c("young_adults", "households", "mean_age", "female", "never_married", paste0("category_", 1:5)),
  unit = c("count", "count", "years", rep("percent", 7)),
  available = profile(available), unavailable = profile(unavailable),
  row.names = NULL)
profiles$difference <- profiles$unavailable - profiles$available
profiles$difference[1:2] <- NA_real_
stopifnot(profiles$available[2] == 3743L, profiles$unavailable[2] == 174L,
  abs(sum(profiles$available[6:10]) - 100) < 1e-10,
  abs(sum(profiles$unavailable[6:10]) - 100) < 1e-10)

stopifnot(dir.create(out, recursive = TRUE))
write.csv(flow, file.path(out, "S12_sample_flow.csv"), row.names = FALSE)
write.csv(profiles, file.path(out, "S12_interview_profiles.csv"), row.names = FALSE, na = "")
print(flow, row.names = FALSE)
print(profiles, row.names = FALSE)

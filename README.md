# Maternal educational constraints and young adult gender attitudes in Türkiye

Replication code and aggregate results for the analysis of linked mother and young adult records in the 2021 Türkiye Family Structure Survey. The primary analysis compares reported family refusal with economic reasons for maternal educational interruption. H3 is an exploratory interaction with the young adult's sex. The four sensitivity models were designed after the initial result. The S12 workflow describes maternal roster coverage and interview availability.

## Source data and access

Apply to the [Turkish Statistical Institute for microdata access](https://tuik.gov.tr/Kurumsal/Mikro_Veri) using its [request portal](https://ty.tuik.gov.tr/). The [TAYA 2021 survey release](https://veriportali.tuik.gov.tr/Bulten/Index?p=Turkiye-Aile-Yapisi-Arastirmasi-2024-45813) describes the study and provides published statistics. Microdata access and reuse are subject to TÜİK's conditions.

Place authorized copies of these files in `data/microdata/` under the repository root. The code checks the byte-level SHA-256 digests before opening the analysis sample.

- `TAYA2021_Fert_MikroVeri.csv`
- `TAYA2021_Fertliste_MikroVeri.csv`
- `TAYA2021_Hane_MikroVeri.csv`

## Software

Use R 4.6.0 and the versions recorded in `R/primary/config.json`. They are survey 4.5, MASS 7.3.65, jsonlite 2.0.0, digest 0.6.39, lpSolve 5.6.23, and knitr 1.51.

## Validation and reproduction

Run the commands from the parent directory of your cloned repository. The two validation commands fit synthetic data and require only the listed software.

```sh
cd taya-maternal-education
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
Rscript --vanilla run.R primary validate
Rscript --vanilla run.R h3 validate
```

After obtaining the authorized source files, use a clean clone with a fresh `output/` directory for reproduction.

```sh
cd taya-maternal-education
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1
Rscript --vanilla run.R primary preflight
Rscript --vanilla run.R primary start
Rscript --vanilla run.R primary verify
Rscript --vanilla run.R h3 preflight
Rscript --vanilla run.R h3 start
Rscript --vanilla run.R h3 verify
Rscript --vanilla run.R s12 output/s12
```

Primary results are written to `output/primary/`. The expected record contains 16 models, including 14 accepted fits and two rejected extreme-threshold diagnostics. H3 verifies the completed local primary run and compares its reference interaction with the stored aggregate H3 estimates and original Holm-adjusted p value. The reference comparison uses a per-field numerical tolerance of `1e-5`. H3 writes four follow-up fits to `output/h3/` and uses a separate Holm correction. S12 writes two descriptive CSV files to `output/s12/`.

Use `primary status` or `h3 status` in place of the stage and mode to inspect progress. Use `resume` in place of `start` to continue from validated checkpoints in the same stage directory. The `verify` modes check the model counts, numerical gates and file manifests. Checkpoints and other local run output belong in `output/`, which Git ignores along with `data/`.

## Included results

- `results/primary/` contains the saved model estimates, original Holm family, sample counts, observed responses, and unweighted composition.
- `results/h3/` contains the interaction and sex-specific contrasts, standardized probabilities, measured covariate support, and household influence summaries.
- `results/s12/` contains the sample flow and weighted profiles for maternal interview availability.

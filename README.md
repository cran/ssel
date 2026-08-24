# ssel

**Semi-Supervised Ensemble Learning**

<!-- badges: start -->
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue)](https://www.r-project.org/)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://averriK.github.io/ssel/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->

`ssel` provides three related method layers for tabular regression:

- weighted ensembles over `caret`-supported learners, with cross-validated
  tuning, out-of-fold diagnostics, signed residual-offset estimates, and two
  explicit response/dataset selection policies;
- iterative multi-response input-space expansion with Jacobi or asymmetric
  Gauss--Seidel companion updates, package-defined fixed or shadow gates, and
  per-response iteration stitching;
- package-defined pseudo-label promotion based on a cross-model and
  cross-dataset range ratio, with an out-of-fold squared-correlation gauge for
  accepting or reverting candidate rounds.

The default supervised learners are random forest (`ranger`), gradient
boosting (`gbm`), and multivariate adaptive regression splines (`earth`). Any
compatible caret method can be supplied through the documented method
arguments.

## Installation

`ssel` requires R >= 4.1.0.

```r
# Released version
install.packages("ssel")
```

## Start here

The quickstart builds a minimal in-memory dataset and introduces the public
entry points:

```r
vignette("ssel-quickstart", package = "ssel")
```

The method articles define the mathematics, package policies, API mapping,
and limitations of each helper family:

```r
vignette("ensemble-theory",  package = "ssel")
vignette("chain-regression", package = "ssel")
vignette("semi-supervised",  package = "ssel")
```

Pipeline entry points compose the lower-level helpers and write their results
to caller-supplied directories. Their exact input schemas, side effects, and
failure conditions are documented in the function reference:

```r
?modelPipeline
?chainPipeline
?semiSupervisedPipeline
```

Rendered documentation: <https://averriK.github.io/ssel/>.

## Public API

Pipeline entry points: `modelPipeline()`, `chainPipeline()`,
`semiSupervisedPipeline()`, `postProcess()`.

Dataset assembly: `buildDataset()`.

Training and prediction: `trainModel()`, `trainRegressionModel()`,
`predictModel()`, `aggregateResponses()`.

OOF reconstruction and diagnostics: `oofEnsemble()`, `auditQuantiles()`,
`auditOverfit()`, `detectOutliers()`.

Companion gates: `extractChainImportance()`, `activeByImportance()`,
`activeByShadow()`, `computeActiveByImportance()`.

Utilities: `toNumeric()`, `which.nonnum()`, `removeOutliersIQR()`.

Full reference: `help(package = "ssel")` or
<https://averriK.github.io/ssel/reference/>.

## Citation

```bibtex
@Manual{VerriKozlowski_ssel,
  title  = {{ssel}: Semi-Supervised Ensemble Learning},
  author = {Verri Kozlowski, Alejandro},
  year   = {2026},
  note   = {R package version 0.3.1},
  url    = {https://averriK.github.io/ssel/}
}
```

## License and contact

MIT; see [LICENSE](LICENSE). The maintainer is Alejandro Verri Kozlowski
([ORCID 0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)),
Universidad de Buenos Aires, Facultad de Ingenieria. Contact information is
listed in `DESCRIPTION`.

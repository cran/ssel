# ssel

**Semi-Supervised Ensemble Learning**

<!-- badges: start -->
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue)](https://www.r-project.org/)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://kanameishi.github.io/ssel/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://kanameishi.github.io/ssel/LICENSE.html)
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

The library and the `ssel` command-line interface are installed together from
the repository root with the product installer, documented in
`install/USAGE.md`. The R package alone, without the command, installs from a
checkout with:

```sh
R CMD INSTALL lib
```

Open the RStudio project inside `lib/` for package development.

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

Rendered documentation: <https://kanameishi.github.io/ssel/>.

## Public API

| Family | Functions | Articles |
| --- | --- | --- |
| Pipeline entry points | `modelPipeline()`, `chainPipeline()`, `semiSupervisedPipeline()`, `postProcess()` | [Ensemble estimators and residual offsets](https://kanameishi.github.io/ssel/articles/ensemble-theory.html); [Iterative multi-response input expansion](https://kanameishi.github.io/ssel/articles/chain-regression.html); [Range-ratio pseudo-label promotion](https://kanameishi.github.io/ssel/articles/semi-supervised.html) |
| Dataset assembly | `buildDataset()` | [Getting started with ssel](https://kanameishi.github.io/ssel/articles/ssel-quickstart.html) |
| Training and prediction | `trainModel()`, `trainRegressionModel()`, `predictModel()`, `aggregateResponses()` | [Ensemble estimators and residual offsets](https://kanameishi.github.io/ssel/articles/ensemble-theory.html) |
| OOF reconstruction and diagnostics | `oofEnsemble()`, `auditQuantiles()`, `auditOverfit()`, `detectOutliers()` | [Ensemble estimators and residual offsets](https://kanameishi.github.io/ssel/articles/ensemble-theory.html) |
| Companion gates | `extractChainImportance()`, `activeByImportance()`, `activeByShadow()`, `computeActiveByImportance()` | [Iterative multi-response input expansion](https://kanameishi.github.io/ssel/articles/chain-regression.html) |
| Utilities | `toNumeric()`, `which.nonnum()`, `removeOutliersIQR()` | [Getting started with ssel](https://kanameishi.github.io/ssel/articles/ssel-quickstart.html) |

Full reference: `help(package = "ssel")` or
<https://kanameishi.github.io/ssel/reference/>.

## Citation

```bibtex
@Manual{VerriKozlowski_ssel,
  title  = {{ssel}: Semi-Supervised Ensemble Learning},
  author = {Verri Kozlowski, Alejandro},
  year   = {2026},
  url    = {https://kanameishi.github.io/ssel/}
}
```

## License and contact

MIT; see [LICENSE](https://kanameishi.github.io/ssel/LICENSE.html). The maintainer is Alejandro Verri Kozlowski
([ORCID 0000-0002-8535-1170](https://orcid.org/0000-0002-8535-1170)),
Universidad de Buenos Aires, Facultad de Ingenieria. Contact information is
listed in `DESCRIPTION`.

# Recommender projects

This repository has different documents, diagrams, code, and projects related to recommendation systems
explanations, development, and utilization.

-------

# Command Line Interface (CLI) scripts

- Launching of Time Series Search Engine (TSSE): ["Launch-TSSE-interface.R"](./CLI/Launch-TSSE-interface.R)

-------

## R scripts

- Creation of data analysis SMR from long form SMR matrix: ["SMR-creation-for-data-analysis.R"](/R/R/SMR-creation-for-data-analysis.R)

-------

## On package licensing

Some loosely adhered to rules for the package licenses in this repository follow.

- The "computer science" packages have BSD-3 or LGPL-3 licenses.

    - For example, the monad code generation package
      [StateMonadCodeGenerator](https://github.com/antononcube/R-packages/tree/master/StateMonadCodeGenerator).

- The more "academic" packages have GPL-3 licenses.

- In general the simple packages have BSD-3 or LGPL-3 licenses.

    - For example, the package ["Launch-TSSE-interface.R"](./CLI/Launch-TSSE-interface.R)
      is (relatively) simple; hence with BSD-3.

- The package license is based on the license(s) of the most important package(s) it builds upon.

    - For example, if a package from this repository is based on one package,
      and the latter is with MIT license,
      then the package from this repository is also with MIT license.

- If a package is with a GPL-3 license that is because:

    - At least one of the underlying important packages is with GPL-3

    - The original version of the package was published with GPL-3

    - GPL-3 is the default and preferred license

-------

## References

### Packages 

[AApR1] Anton Antonov,
[Sparse Matrix Recommender framework interface functions](https://github.com/antononcube/R-packages/tree/master/SparseMatrixRecommenderInterfaces)
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).

[AApR2] Anton Antonov,
[Sparse Matrix Recommender Monad in R](https://github.com/antononcube/R-packages/tree/master/SMRMon-R)
(2019-2022),
[R-packages at GitHub](https://github.com/antononcube/R-packages).

[AApR3] Anton Antonov,
[Latent Semantic Analysis Monad in R](https://github.com/antononcube/R-packages/tree/master/LSAMon-R)
(2019),
[R-packages at GitHub](https://github.com/antononcube/R-packages).

[AApWL1] Anton Antonov,
[Sparse matrix recommender framework in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/SparseMatrixRecommenderFramework.m),
(2014),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AApWL2] Anton Antonov,
[Monadic Sparse Matrix Recommender Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicSparseMatrixRecommender.m),
(2018-2022),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).


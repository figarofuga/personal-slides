#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT

cat > "$scratch/check.qmd" <<'EOF'
---
title: "Environment check"
format: html
engine: knitr
execute:
  cache: false
  freeze: false
---

```{r}
stopifnot(grepl("renv/library", find.package("ggplot2"), fixed = TRUE))
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point()
```

```{python}
import numpy as np
import pandas as pd
assert np.arange(5).sum() == 10
print(pd.DataFrame({"value": [1, 2, 3]}))
```
EOF

R_PROFILE_USER="$project_root/.Rprofile" quarto render "$scratch/check.qmd"
test -s "$scratch/check.html"
echo "Quarto execution with R, graphics, and Python: OK"

# Using lfs to track large files

## Installation
```bash
brew install git-lfs
git lfs install
```

## Commit and push
```bash
git lfs track "*.feather"
git add .gitattributes
git add -A
git commit -m "add cleaned data"
git push origin master
```


## Load and write feather files

### R

```R
library(feather)
path <- "my_data.feather"
write_feather(df, path)
df <- read_feather(path)
```

### Python

```python
import feather
path = 'my_data.feather'
feather.write_dataframe(df, path)
df = feather.read_dataframe(path)
```
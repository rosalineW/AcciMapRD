# AcciMapRD

The goal of AcciMapRD is helping users to manipulate the data of accidents happend in the US in certain years. It includes the functions of searching available datasets in the package, summarizing the number of accident, mapping the accident point, and so on.

## Example

This is an example which shows you how to check the existence of the accident record in 2014:

```{r}
fars_read(make_filename(2014))
```

This is an example which shows you how to map the points where the accidents happend at the state whose state number is 1 in 2015.

```{r}
fars_map_state(1,2015)
```

This is an example which shows you how to generate a summary table of the number of accident happend in each month of 2013, 2014, and 2015.

```{r}
fars_summarize_years(c(2013,2014,2015))
```

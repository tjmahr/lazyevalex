lazyeval exercises
================
Tristan Mahr
June 18, 2016

Part 1
------

> Create a wrapper around lm() that allows the user to supply the response and predictors as two separate formulas.

``` r
lm2 <- function(response, predictor, df) {
  # I can't figure out how to uq both at once within a formula that uses a
  # formula (~ lm(y ~ x)), so I'll plug them into a formula one by one.
  f <- y ~ x
  f_lhs(f) <- uq(response)
  f_rhs(f) <- uq(predictor)

  f_eval(~ lm(uqf(f), df))
}

lm2(~ mpg, ~ cyl, mtcars)
#> 
#> Call:
#> lm(formula = mpg ~ cyl, data = df)
#> 
#> Coefficients:
#> (Intercept)          cyl  
#>      37.885       -2.876

lm2(~ mpg, ~ hp, mtcars)
#> 
#> Call:
#> lm(formula = mpg ~ hp, data = df)
#> 
#> Coefficients:
#> (Intercept)           hp  
#>    30.09886     -0.06823
```

I don't know how I would incorporate `...` yet. I'm not that far in the vignette.

------------------------------------------------------------------------

> Compare and contrast f\_eval() with with().

Both can use names outside of the data.

``` r
cyl_sq <- mtcars$cyl ^ 2

with(mtcars, coef(lm(mpg ~ cyl + cyl_sq)))
#> (Intercept)         cyl      cyl_sq 
#>  47.3389610  -6.3077922   0.2847403

f_eval(~ coef(lm(mpg ~ cyl + cyl_sq)), mtcars)
#> (Intercept)         cyl      cyl_sq 
#>  47.3389610  -6.3077922   0.2847403
```

I thought `with` would work better with blocks of multiple lines of code, so `f_eval` can do those too.

``` r
with(mtcars, {
  log_wt <- log(wt)
  mean(log_wt)
})
#> [1] 1.121739

f_eval(~ {
  log_wt <- log(wt)
  mean(log_wt)
}, mtcars)
#> [1] 1.121739
```

The main difference is that `f_eval` provides an unquoting/interpolation step in the middle.

``` r
some_var <- ~ wt
with(mtcars, mean(uq(some_var)))
#> Warning in mean.default(uq(some_var)): argument is not numeric or logical:
#> returning NA
#> [1] NA

f_eval(~ mean(uq(some_var)), mtcars)
#> [1] 3.21725
```

`with` jumps right into evaluation (`eval`).

``` r
with.default
#> function (data, expr, ...) 
#> eval(substitute(expr), data, enclos = parent.frame())
#> <bytecode: 0x00000000142e4698>
#> <environment: namespace:base>
```

`f_eval` modifies the expression with `f_interp` before calling its evaluation step (`eval_expr`).

``` r
f_eval
#> function (f, data = NULL) 
#> {
#>     if (!is_formula(f)) {
#>         stop("`f` is not a formula", call. = FALSE)
#>     }
#>     expr <- f_rhs(f_interp(f, data = data))
#>     eval_expr(expr, f_env(f), data)
#> }
#> <environment: namespace:lazyeval>
```

------------------------------------------------------------------------

> Why does this code work even though f is defined in two places? (And one of them is not a function).
>
> ``` r
> f <- function(x) x + 1
> f_eval(~ f(10), list(f = "a"))
> #> [1] 11
> #> [1] 11
> ```

The same reason this can work coherently:

``` r
log(100)
#> [1] 4.60517
log <- 100
log(100)
#> [1] 4.60517
log(log)
#> [1] 4.60517
```

I don't know the low-level eval/apply steps in detail, but the interpreter looks for a function with the name `f` when evaluating a function call by `f`, so it skips over the piece of data named `f` that is not a function.

``` r
f <- function(x) x + 1
f_eval(~ f(10))
#> [1] 11
f_eval(~ f(10), data = list(f = "a"))
#> [1] 11

# Put a function in the data
f_eval(~ f(10), data = list(f = sqrt))
#> [1] 3.162278
```

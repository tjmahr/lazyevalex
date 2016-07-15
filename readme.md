lazyeval exercises
================
Tristan Mahr
June 18, 2016

Hadley Wickham has included exercises in the [lazyeval vignette](https://cran.rstudio.com/web/packages/lazyeval/vignettes/lazyeval.html). This is a great idea. Normally, when reading a vignette, I spend most of my time and attention playing with the example code. Exercises encourage this exploration by giving the reader problems to solve. Let's hope he's not [trolling us here](https://twitter.com/hadleywickham/status/743177000349667328).

Part 1: Labeling
----------------

### Notes

`expr_text(x)` replaces `deparse(substitute(x))` idiom for capturing expressions for labeling. `expr_label` is a more suitable version for printing messages.

`expr_find` improves on `substitute` by finding the original expression used for a value; `substitute` only looks in its parent environment.

``` r
f <- function(x) g(x)
g <- function(y) h(y)
h <- function(z) list(substitute = substitute(z), expr_find = expr_find(z))

f(1 + 2 + 3)
#> $substitute
#> y
#> 
#> $expr_find
#> 1 + 2 + 3
```

### Exercises

**`plot()` uses `deparse(substitute(x))` to generate labels for the x and y axes. Can you generate input that causes it to display bad labels?**

Wrap the function so the user's arguments are stored in an intermediate variable. That will undermine `substitute`'s search.

``` r
par(mar = c(4.5, 4.5, 1, 0.5))
grid <- seq(0, 2 * pi, length = 100)

line_plot <- function(x, y, ...) {
  plot(x, y, type = "l", ...)
}

line_plot(grid, sin(grid))
```

![](readme_files/figure-markdown_github/unnamed-chunk-2-1.png)

**Write your own wrapper around `plot()` that uses `expr_label()` to compute `xlim` and `ylim`.**

Assuming the question means `xlab` and `ylab`...

``` r
line_plot2 <- function(x, y, ...) {
  plot(x, y, type = "l", xlab = expr_label(x), ylab = expr_label(y), ...)
}

line_plot2(grid, sin(grid))
```

![](readme_files/figure-markdown_github/unnamed-chunk-3-1.png)

**Create a simple implementation of `mean()` that stops with an informative error message if the argument is not numeric.**

``` r
x <- 1:100
my_mean <- function(x, ...) {
  if (!is.numeric(x)) {
    stop(expr_label(x), " is not a numeric vector.", call. = FALSE)  
  }
  mean(x, ...)
}

x <- c("a", "b", "c")
my_mean(x)
#> Error: `x` is not a numeric vector.
my_mean(x == "a")
#> Error: `x == "a"` is not a numeric vector.
my_mean("a")
#> Error: "a" is not a numeric vector.
```

**Read the source code for `expr_text()`. How does it work? What additional arguments to `deparse()` does it use?**

Step one: It finds the original expression, then dispatches to a standard evaluation version of the function.

``` r
expr_text
#> function (x, width = 60L, nlines = Inf) 
#> {
#>     expr_text_(expr_find(x), width = width, nlines = nlines)
#> }
#> <environment: namespace:lazyeval>
```

Step two: It deparses the expression(s) into text and truncates it. The function cuts off all text beyond a given number of characters (60 by default). Deparsing produces a vector of text when the expression spans multiple lines. This function can optionally cut off all text beyond a certain number lines. All the lines of text are collapsed into a single string.

``` r
lazyeval:::expr_text_
#> function (x, width = 60L, nlines = Inf) 
#> {
#>     str <- deparse(x, width.cutoff = width)
#>     if (length(str) > nlines) {
#>         str <- c(str[seq_len(nlines - 1)], "...")
#>     }
#>     paste0(str, collapse = "\n")
#> }
#> <environment: namespace:lazyeval>
```

Part 2: Formulas
----------------

### Exercises

**Create a wrapper around `lm()` that allows the user to supply the response and predictors as two separate formulas.**

``` r
lm2 <- function(response, predictor, data, ...) {
  # I can't figure out how to uq both at once within a formula that uses a
  # formula (~ lm(y ~ x)), so I'll plug them into a formula one by one.
  f <- y ~ x
  f_lhs(f) <- uq(response)
  f_rhs(f) <- uq(predictor)

  lm(formula = f, data = data, ...)
}

lm2(~ mpg, ~ cyl, mtcars)
#> 
#> Call:
#> lm(formula = f, data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl  
#>      37.885       -2.876
lm2(~ mpg, ~ hp * cyl, mtcars)
#> 
#> Call:
#> lm(formula = f, data = data)
#> 
#> Coefficients:
#> (Intercept)           hp          cyl       hp:cyl  
#>    50.75121     -0.17068     -4.11914      0.01974
```

**Compare and contrast `f_eval()` with `with()`.**

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

I thought `with` would work better with blocks of (multiple lines of) code, because that's how I've seen `with` used. But `f_eval` can do those too.

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

This step difference is transparent in the function code. `with` jumps right into evaluation (`eval`).

``` r
with.default
#> function (data, expr, ...) 
#> eval(substitute(expr), data, enclos = parent.frame())
#> <bytecode: 0x0000000006449918>
#> <environment: namespace:base>
```

`f_eval` modifies the expression with `f_interp` before calling the evaluation step (`eval_expr`).

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

**Why does this code work even though `f` is defined in two places? (And one of them is not a function).**

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

I don't know the low-level [eval/apply](https://groups.csail.mit.edu/mac/classes/6.001/abelson-sussman-lectures/wizard.jpg) steps for R in detail, but the interpreter looks for *a function* with the name `f` when evaluating a function call by `f`, so it skips over the piece of data named `f` that is not a function.

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

Part 3: Non-standard scoping
----------------------------

### Exercises

**Write a function that selects all rows of `df` where variable is greater than its mean. Make the function more general by allowing the user to specify a function to use instead of `mean()` (e.g. `median()`).**

Because we know the function is going to be `mean` we can add an assertion that throws in an error if the variable has the wrong class.

``` r
above_average <- function(df, variable) {
  # variable should be average-able
  var_class <- f_eval(~ class(uq(variable)), df)
  if (!(var_class %in% c("logical", "numeric"))) {
    # Part 1
    stop(expr_label(variable), " cannot be averaged.", call. = FALSE)
  }
  
  rows <- f_eval(~ mean(uq(variable)) < uq(variable), df)
  rows[is.na(rows)] <- FALSE
  df[rows, , drop = FALSE]
}

mtcars$Model <- row.names(mtcars)
row.names(mtcars) <- NULL

above_average(mtcars, ~ Model)
#> Error: `~Model` cannot be averaged.

above_average(mtcars, ~ hp)
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb               Model
#> 5  18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2   Hornet Sportabout
#> 7  14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4          Duster 360
#> 12 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3          Merc 450SE
#> 13 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3          Merc 450SL
#> 14 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3         Merc 450SLC
#> 15 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4  Cadillac Fleetwood
#> 16 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4 Lincoln Continental
#> 17 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4   Chrysler Imperial
#> 22 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2    Dodge Challenger
#> 23 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2         AMC Javelin
#> 24 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4          Camaro Z28
#> 25 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2    Pontiac Firebird
#> 29 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4      Ford Pantera L
#> 30 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6        Ferrari Dino
#> 31 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8       Maserati Bora

# bottom half
above_average(mtcars, ~ -hp)
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb          Model
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4      Mazda RX4
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4  Mazda RX4 Wag
#> 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1     Datsun 710
#> 4  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 Hornet 4 Drive
#> 6  18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1        Valiant
#> 8  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2      Merc 240D
#> 9  22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2       Merc 230
#> 10 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4       Merc 280
#> 11 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4      Merc 280C
#> 18 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1       Fiat 128
#> 19 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2    Honda Civic
#> 20 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 Toyota Corolla
#> 21 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1  Toyota Corona
#> 26 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1      Fiat X1-9
#> 27 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  Porsche 914-2
#> 28 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2   Lotus Europa
#> 32 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2     Volvo 142E
```

In the general case, we need to look up the function in the environment, not the data. We will also add in dots to support for function argument like `na.rm`. The main function fits on one-line, but it is pretty tough to read.

``` r
above_f <- function(df, variable, f, ...) {
  rows <- f_eval(~ .env$f(uq(variable), ...) < uq(variable), df)
  rows[is.na(rows)] <- FALSE
  df[rows, , drop = FALSE]
}

above_f(mtcars, ~ mpg, median)
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb          Model
#> 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4      Mazda RX4
#> 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4  Mazda RX4 Wag
#> 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1     Datsun 710
#> 4  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1 Hornet 4 Drive
#> 8  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2      Merc 240D
#> 9  22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2       Merc 230
#> 18 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1       Fiat 128
#> 19 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2    Honda Civic
#> 20 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 Toyota Corolla
#> 21 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1  Toyota Corona
#> 26 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1      Fiat X1-9
#> 27 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  Porsche 914-2
#> 28 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2   Lotus Europa
#> 30 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6   Ferrari Dino
#> 32 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2     Volvo 142E
above_f(mtcars, ~ mpg, function(xs, probs) quantile(xs, probs = probs), .75)
#>     mpg cyl  disp  hp drat    wt  qsec vs am gear carb          Model
#> 8  24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2      Merc 240D
#> 18 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1       Fiat 128
#> 19 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2    Honda Civic
#> 20 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1 Toyota Corolla
#> 26 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1      Fiat X1-9
#> 27 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2  Porsche 914-2
#> 28 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2   Lotus Europa
above_f(mtcars, ~ mpg, function(xs, probs) quantile(xs, probs = probs), .95)
#>     mpg cyl disp hp drat    wt  qsec vs am gear carb          Model
#> 18 32.4   4 78.7 66 4.08 2.200 19.47  1  1    4    1       Fiat 128
#> 20 33.9   4 71.1 65 4.22 1.835 19.90  1  1    4    1 Toyota Corolla
above_f(mtcars, ~ mpg, max)
#>  [1] mpg   cyl   disp  hp    drat  wt    qsec  vs    am    gear  carb 
#> [12] Model
#> <0 rows> (or 0-length row.names)
```

**Create a version of `mogrify()` where the first argument is `x`? What happens if you try to create a new variable called `x`?**

This is the original version of the `mogrify` function. The first argument has a special syntactically obscured name to avoid matching any of the generated column names. This exercise asks us to remove this safeguard. The expected behavior is given below:

``` r
mogrify1 <- function(`_df`, ...) {
  args <- f_list(...)
  for (nm in names(args)) `_df`[[nm]] <- f_eval(args[[nm]], `_df`)
  `_df`
}

df <- data.frame(y = 1:5)
mogrify1(df, z = ~ y * 2, x = ~ y * -1)
#>   y  z  x
#> 1 1  2 -1
#> 2 2  4 -2
#> 3 3  6 -3
#> 4 4  8 -4
#> 5 5 10 -5
```

This is the updated version. I added a `str(args)` line to debug the structure of the named list being looped over.

``` r
mogrify2 <- function(x, ...) {
  args <- f_list(...)
  str(args)
  for (nm in names(args)) x[[nm]] <- f_eval(args[[nm]], x)
  x
}
```

We get an error when get try to create a column named `x`. The error that `f` is not a formula matches the `str(args)` print-out, because it shows a data-frame as one of the elements in `args`.

``` r
rm(x)
mogrify2(df, z = ~ y * 2, x = ~ y * -1)
#> List of 2
#>  $  :'data.frame':   5 obs. of  1 variable:
#>   ..$ y: int [1:5] 1 2 3 4 5
#>  $ z:Class 'formula'  language ~y * 2
#>   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv>
#> Error: `f` is not a formula
```

The problem is that the function arguments are being matched by name, not by position. The same problem is on display here:

``` r
f <- function(a, ...) c(a, ...)
f(1, 2, 3, 100)
#> [1]   1   2   3 100
f(1, 2, 3, a = 100)
#> [1] 100   1   2   3
```

Part 4: Non-standard evaluation
-------------------------------

### Exercises

**Recreate `subscramble()` using `base::subset()` instead of `sieve()`. Why does it fail?**

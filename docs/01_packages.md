
# R package with unit tests {#packages}

## Learning objectives

* Create an R package
* Create a function
* Document the function
* Include error checking in the function
* Write unit tests for the function
* Use your package in a script
* Share your package through GitHub

## Setting up

You will need to install the following <a class='glossary' target='_blank' title='A group of R functions.' href='https://psyteachr.github.io/glossary/p#package'>packages</a>:


```r
install.packages(c("devtools", "roxygen2", "testthat", "usethis", "knitr"))
```


## Create your R package

Use the following command to create the framework for a new package called `demopckg`. Set the argment to the path where you want to save your package. The last section of the path should be the name of the package.


```r
usethis::create_package("~/rstuff/demopckg")
```

<div class="warning">
<p>Package names can only be letters, numbers, and full stops.</p>
</div>

You'll see the following output, and a new RStudio <a class='glossary' target='_blank' title='A way to organise related files in RStudio' href='https://psyteachr.github.io/glossary/p#project'>project</a> will open up. You can close the old window now and just work in this project.

```
✔ Setting active project to '~/rstuff/demopckg'
✔ Creating 'R/'
✔ Creating 'man/'
✔ Writing 'DESCRIPTION'
✔ Writing 'NAMESPACE'
✔ Writing 'demopckg.Rproj'
✔ Adding '.Rproj.user' to '.gitignore'
✔ Adding '^demopckg\\.Rproj$', '^\\.Rproj\\.user$' to '.Rbuildignore'
✔ Opening new project 'demopckg' in RStudio
```

### Edit the DESCRIPTION file

Open the `DESCRIPTION` file. It should look like this:

```
Package: demopckg
Title: What the Package Does (One Line, Title Case)
Version: 0.0.0.9000
Authors@R: 
    person(given = "First",
           family = "Last",
           role = c("aut", "cre"),
           email = "first.last@example.com")
Description: What the package does (one paragraph).
License: What license it uses
Encoding: UTF-8
LazyData: true
```

Change the title, authors, and description to your own information. If your package has multiple authors, add them as a vector like this. You can also add your [ORCiD](https://orcid.org/).

```
Authors@R: c(
    person(given = "Lisa",
           family = "DeBruine",
           role = c("aut", "cre"),
           email = "debruine@gmail.com",
           comment = c(ORCID = "0000-0002-7523-5539")),
    person(given = "Firstname",
           family = "Lastname",
           role = c("aut"),
           email = "person@gmail.com",
           comment = c(ORCID = "0000-0000-0000-000X")))
```


### Create a LICENSE

Add a license using one of the following options:


```r
usethis::use_mit_license(name = "YOUR NAME")  # permissive sharing
usethis::use_cc0_license(name = "YOUR NAME")  # public domain - use for data packages
usethis::use_gpl3_license(name = "YOUR NAME") # derivatives must be open
```

### Create a README

Use the following code to set up a README document that will explain your package.


```r
usethis::use_readme_rmd() 
```

We'll eventually put this on <a class='glossary' target='_blank' title='A cloud-based service for storing and sharing your version controlled files.' href='https://psyteachr.github.io/glossary/g#github'>github</a>, so change the installation instructions to the following (change `yourusername` to your github username). 

<pre><code>You can install the released version of demopckg from [GitHub](https://github.com) with:

&#96;&#96;&#96; r
devtools::install_github("yourusername/demopckg")
&#96;&#96;&#96;</code></pre>

Delete the example for now.

Make sure you knit your README.Rmd file when you update it and never edit the README.md file (that's just for github).

## Creating a function

Function definitions are saved in the `R` folder. You don't have to, but I like to save each function in its own file and name the file after the function.

### Template function

Create a new R script from the File menu (**`New File > R Script`**).

Paste the following template into your file:


```r
myfunction <- function(arg1 = "Change me") { 
  arg1 
}
```


### Edit the function

We're going to create a <a class='glossary' target='_blank' title='A named section of code that can be reused.' href='https://psyteachr.github.io/glossary/f#function'>function</a> that reports a p-value in APA style, named `report_p`. It will take two <a class='glossary' target='_blank' title='A variable that provides input to a function.' href='https://psyteachr.github.io/glossary/a#argument'>arguments</a>, the p-value (`p`) and the number of digits to round to (`digits`). 

<div class="try">
<p>Replace <code>myfunction</code> with <code>report_p</code> and change the arguments. Should <code>p</code> have a default value? Should <code>digits</code>?</p>
</div>

The first thing we should do in the function is check whether `p` is less than 0.001, and if it is, return the value "p < .001".


```r
report_p <- function(p) {
  if (p < .001) return("p < .001")
}
```

<div class="info">
<p>Once you call the <code>return()</code> function, your function stops running.</p>
</div>

If `p` is greater than 0.001, then we should round it to the specified number of `digits`, paste it after the string "p = ", and return it. So add an argument called `digits` and set the default value to 3.


```r
report_p <- function(p, digits = 3) {
  if (p < .001) return("p < .001")
  
  p_round <- round(p, digits)
  p_string <- paste("p =", p_round)
  
  return(p_string)
}
```

<div class="try">
<p>Run your function and test it with a few different p-values and digits. Try <code>report_p(0.01034)</code>. Does this look exactly like you expect?</p>
</div>

APA style omits the leading zero and pads the number out to three digits. We can do this by converting our rounded p-value into a <a class='glossary' target='_blank' title='A data type representing strings of text.' href='https://psyteachr.github.io/glossary/c#character'>character</a> string, replacing the string "0." with ".", and making sure to pad the right side with enough zeros. The `stringr` package has useful functions for this.

When you use R functions from a package (not base R), you normally load the package using the `library()` function. When you're developing your own package, you should preface every function with its package name and two colons instead, so in the code below we'll use `stringr::str_replace()` and `stringr::str_pad()`, not `str_replace()` and `str_pad()`.

<div class="info">
<p>One function you can’t preface with the package name is the <a href="#pipes">pipe</a>. While you’re testing your function, load the pipe by typing <code>library(magrittr)</code> in the console.</p>
</div>


```r
report_p <- function(p, digits = 3) {
  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)
  
  return(p_string)
}
```


### Documentation

Put your cursor somewhere on the first line of your function and choose **`Insert Roxygen Skeleton`** from the **`Code`** menu. 

<div class="figure" style="text-align: center">
<img src="images/roxygen.png" alt="Insert Roxygen Skeleton" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-7)Insert Roxygen Skeleton</p>
</div>

It will insert the following documentation before your function.


```r
#' Title
#'
#' @param p 
#' @param digits 
#'
#' @return
#' @export
#' 
#' @examples
```

The `#'` is special to `roxygen2` documentation, which we'll enable below. This generates what you see in the Help viewer. Type `?mean` into the console pane and have a look at the Help pane.

* The first line is the name of the function in title case
* The Description is the lines between the title and first `@param`
* The Useage is automatically generated
* The Arguments section is generated from the list of `@param`argument Argument description...
* The Value section is the text after `@return`
* The Examples section is the text under `@examples`
* This block should include `@export` to make sure your function is available for users of your package (replace `@export` with `@keywords internal`

The documentation for your `report_p` function should look something like below:


```r
#' Report p-value
#'
#' Reports a p-value in APA style.
#'
#' @param p The p-value
#' @param digits The number of digits to round to (default = 3)
#'
#' @return A string with the format "p = .040" or "p < .001"
#' @examples
#'
#' report_p(0.02018) # returns "p = .020"
#' report_p(0.00028) # returns "p < .001"
#'
#' @export
```

Save your file in the `R` directory with the name `report_p.R`. For now, we'll make a separate file for each function and give it the name of the function.

Roxygen creates automatic documentation. You enable it with the following command (you only need to run this once per package).


```r
usethis::use_roxygen_md()
```

Now you can automatically update the documentation for your package by running `devtools::document()`, after which you should see the following text.


```r
devtools::document()
```

```
Updating demopckg documentation
Writing NAMESPACE
Loading demopckg
Writing report_p.Rd
```

You don't need to worry about these files, they'll be added to your package to show Help documentation.


### Imports

You need to "import" any packages you used in your function by running `usethis::use_package` for each package you want to include. 


```r
usethis::use_package("stringr")
```

You can't import the whole `tidyverse`, but you can import each package separately (i.e., ggplot2, purrr, tibble, dplyr, tidyr, stringr, readr, forcats). Import just the packages you actually need.

<div class="warning">
<p><img src="images/pipe_sticker.png" style="float:right; width:100px"> If you use pipes (even if you’ve imported <code>dplyr</code>), you also need to run <code>usethis::use_pipe()</code>. It will add a file called <code>utils-pipe.R</code> to your <code>R</code> directory and add <code>magrittr</code> to your Imports.</p>
</div>


## Build your package

Now you're ready to check and build your package for installation.

### Check

First, check everything by running `devtools::check()`. You'll get a lot of output, but don't worry unless you have an error message. Hopefully, you'll get this message:

```
0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```
### Build

Once you've fixed any errors, you can build your package.


```r
devtools::build()
```

You'll get a message that looks like this:

```
✔  checking for file ‘/Users/lisad/rproj/demopckg/DESCRIPTION’ ...
─  preparing ‘demopckg’:
✔  checking DESCRIPTION meta-information ...
─  checking for LF line-endings in source and make files and shell scripts
─  checking for empty or unneeded directories
─  building ‘demopckg_0.0.0.9000.tar.gz’
   
[1] "/Users/lisad/rproj/demopckg_0.0.0.9000.tar.gz"
```
### Install

Next, install your new package with the following code (`../`  means to go up one directory to look for the `demopckg` install file).


```r
devtools::install("../demopckg")
```

You'll see a bunch of output that should end in:

```
* DONE (demopckg)
```

### Test

To make sure it's all gone well, restart R and try to use the function `report_p(0.00039)`. You should get an error message.

Then load your new package with `library(demopckg)` are retry the example above.

Type `?report_p` in the console and look at your Help documentation.

<div class="try">
<p>Restart R and open a new .R or .Rmd file. Load your new package at the top of the file and try using your function in a paragraph that reports the p-value for a test.</p>
</div>

## Error checking

Try running your function with different values for `p`. What happens if you use invalid values, like 1.07, -0.05, or "A"?

We can add error checking to a function to quit and give a message if the error is fatal, or just warn them if the error is recoverable, but probably wrong.

P-values can't ever be less than 0 or greater than 1, so we can just quit and give an error message if that happens. Add the following code to the beginning of your function, rerun the code to update the function, and test it on some values of p that are less than 0 or greater than 1.


```r
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")
```

What other errors do you think people might make? You can add checks to the beginning of your function to warn people if they don't enter reasonable numbers for the digits argument and set digits to the default value so that the code can continue.


```r
  if (!(digits %in% 1:5)) {
    warning("digits should probably be an integer between 1 and 5")
    digits = 3
  }
```

## Unit tests

Up until now, we've just tested our function in an ad hoc way every time we made changes. We can make this process more formal by using unit tests. These will make sure that your function is working properly if you make any changes. This seems like overkill for simple functions, but is absolutely essential for big projects, so best to get into good habits now.

### Setup

When you set up a new package, you need to set up the testing structure using `usethis::use_testthat()`. You only need to do this once for each package and you will know it has been done if a new directory called `tests` is made.

### New unit tests

Create a new test file for the `report_p` function using `usethis::use_test("report_p")`. It will create a new file called **`test-report_p.R`** in the  **`tests/testhat/`** directory.

Replace the text in that file with the text below.


```r
context("report_p")

testthat::test_that("errors", {
  testthat::expect_error(
    report_p(-1),
    "p cannot be less than 0"
  )
  
  testthat::expect_error(
    report_p(2),
    "p cannot be greater than 1"
  )
})
```

The `context` function lets you know what function you're testing when you run all the unit tests in a package. The `test_that` function checks a groups of expectations. The first set checks if we get the error messages we expect, so we've called it "errors".

We're going to check two expectations, that we'll get the error message "p cannot be less than 0" if p = -1, and that we'll get the error message "p cannot be greater than 1" id p = 2. You can test more values than these, but we'll start with just these two.

After you save this file, run `devtools::test()`. You should see output like:

```
Loading demopckg
Testing demopckg
✔ | OK F W S | Context
✔ |  2       | report_p

══ Results ═══════════════════════════════════════════════════════════
OK:       2
Failed:   0
Warnings: 0
Skipped:  0
```

Now add another `testthat::test_that` block called "default values". Use the function `testthat::expect_equal` to check if the output of the `report_p()` function with different `p` values and the default `digits` value gives you the expected output. For example:


```r
  testthat::expect_equal(
    report_p(p = 0.0222),
    "p = .022"
  )
```

### Run all tests

Run `devtools::test()` after you add each test to make sure your tests work as expected.

## Documentation

Now it's time to edit your README to explain how to use your package.

### Vignettes

A [vignette](http://r-pkgs.had.co.nz/vignettes.html) can also help users understand how to use your package. Create a new vignette with the function below.


```r
usethis::use_vignette("example")
```

Change the YAML header to your "Vignette Title" and "Vignette Author" and edit the text below using R Markdown.

### pkgdown

The package [pkgdown](https://pkgdown.r-lib.org/) creates a website for your package. Run the following function to set it up. 


```r
usethis::use_pkgdown()
```

This will create a directory called `docs` and a file called `_pkgdown.yml`.

Whenever you edit the documentation for new function, edit the README, or edit a vignette, you need to re-build the site.


```r
pkgdown::build_site()
```

There will be a lot of output, then your site should automatically open up in your default web browser.

## Share your package

You can do package development without a [GitHub](https://github.com) account, but this is one of the easiest ways to share your package.

### Git on RStudio

If you don't have <a class='glossary' target='_blank' title='One type of version control software.' href='https://psyteachr.github.io/glossary/g#git'>git</a> installed on your computer, don't have it integrated with RStudio, and/or don't have a github account, follow the instructions in Appendix \@ref(#git).

### Set up git for this project

If you aren't already using <a class='glossary' target='_blank' title='A way to save a record of changes to your files.' href='https://psyteachr.github.io/glossary/v#version-control'>version control</a> for this project, make sure all of your files are saved and type `usethis::use_git()` into the console. Choose Yes to commit and Yes to restart R.

### GitHub access token

Now set up a github access token with `usethis::browse_github_pat()`. Your web browser will open and you'll be asked to log into your github account and then asked to authorise a new token. Accept the defaults and click OK at the bottom of the page.

<div class="figure" style="text-align: center">
<img src="images/github_new_token.png" alt="Authorise a github token so you can create new github repositories from RStudio" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-13)Authorise a github token so you can create new github repositories from RStudio</p>
</div>

Copy your token (the whited-out bit in the image below).

<div class="figure" style="text-align: center">
<img src="images/github_copy_token.png" alt="Copy your github token" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-14)Copy your github token</p>
</div>

Type `usethis::edit_r_environ()` in the RStudio console pane. A new file called `.Renviron` will appear in the source pane. Add the following line to it (replace **`<YOUR-TOKEN>`** with your copied token).

```
GITHUB_PAT=<YOUR-TOKEN>
```

Save and close the file, then restart R.

### Make a new GitHub repository

Type `usethis::use_github(protocol="https")` into the console window and check that the suggested title and description are OK.

```
✔ Setting active project to '/Users/lisad/rstuff/demopckg'
● Check title and description
  Name:        demopckg
  Description: Demo Stuff
Are title and description ok?
1: No way
2: Yeah
3: No
```

If you choose the affirmative response (not always #2), you'll see some messages and your web browser will open the github repsitory page.

<div class="figure" style="text-align: center">
<img src="images/github_demoproj.png" alt="Your new github repository" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-15)Your new github repository</p>
</div>

### Set up website

Click on the **`Settings`** tab and scroll down to **GitHub Pages**. Set the Source to **`master branch/docs folder`**. Now you should be able to access your pkgdown website from https://username.github.io/demopkg/ (change to your username).

<div class="figure" style="text-align: center">
<img src="images/github_pages.png" alt="Enable GitHub Pages" width="100%" />
<p class="caption">(\#fig:unnamed-chunk-16)Enable GitHub Pages</p>
</div>

### Install your package from GitHub

Install your package using the following code (replacing `username` with your github username).


```r
devtools::install_github("username/demopckg")
```


## Further resources

There is a lot more to learn about package development, including writing vignettes to help users understand your functions and getting your package ready to submit to CRAN.

* [R Packages](http://r-pkgs.had.co.nz/) by Hadley Wickham
* [usethis](https://usethis.r-lib.org/)
* [Workflow for package development](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/#creating-minimal-functional-package) by Emil Hvitfeldt

### Workflow

The following script has all of the functions you'll need to start a new package.


```r
pckg <- "mynewpackage"
pckgdir <- "~/rproj/"
me <- "Lisa DeBruine"

# run once at start of package
usethis::create_package(paste0(pckgdir, pckg))
usethis::use_mit_license(name = me)
usethis::use_readme_rmd()
usethis::use_testthat()
usethis::use_roxygen_md()
usethis::use_pipe() # everyone needs pipes

# code for new functions
funcname <- "newfunction"
imports <- c("dplyr", "tidyr")
usethis::edit_file(paste0("R/", funcname, ".R"))
for (import in imports) usethis::use_package(import)

# testing a function
usethis::use_test(funcname)
devtools::test(filter = funcname)

# documentation
usethis::use_vignette("example")
usethis::use_pkgdown()
pkgdown::build_site()

# building the package
devtools::check() # can take a long time
devtools::build()
devtools::install(paste0("../", pckg))

# use these for specific tasks 
# if check() or build_site() take too long
devtools::document()       # updates from roxygen function documentation
devtools::test()           # runs all your unit tests, or use filter
devtools::run_examples()   # checks all your function examples work
pkgdown::build_home()      # from README and DESCRIPTION
pkgdown::build_article()   # from vignettes
pkgdown::build_reference() # from roxygen function documentation
```



### The full `report_p` function

Here's what the full function should look like.


```r
#' Report p-value
#'
#' Reports a p-value in APA style.
#'
#' @param p The p-value
#' @param digits The number of digits to round to (default = 3)
#'
#' @return A string with the format "p = .040" or "p < .001"
#' @examples
#'
#' report_p(0.02018) # returns "p = .020"
#' report_p(0.00028) # returns "p < .001"
#'
#' @export

report_p <- function(p, digits = 3) {
  if (p < 0) stop("p cannot be less than 0")
  if (p > 1) stop("p cannot be greater than 1")
  if (digits < 1) {
    warning("digits should probably be an integer between 1 and 5")
    digits = 3
  }
  
  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)
  
  return(p_string)
}
```

## Glossary



term                                                                                                                    definition                                                                   
----------------------------------------------------------------------------------------------------------------------  -----------------------------------------------------------------------------
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/a#argument'>argument</a>                 A variable that provides input to a function.                                
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/c#character'>character</a>               A data type representing strings of text.                                    
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/f#function.'>function </a>               A named section of code that can be reused.                                  
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/g#git'>git</a>                           One type of version control software.                                        
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/g#github'>github</a>                     A cloud-based service for storing and sharing your version controlled files. 
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/p#package'>package</a>                   A group of R functions.                                                      
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/p#project'>project</a>                   A way to organise related files in RStudio                                   
<a class='glossary' target='_blank' href='https://psyteachr.github.io/glossary/v#version.control'>version control</a>   A way to save a record of changes to your files.                             



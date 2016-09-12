## Resubmission

## Submiting new version 0.2.12 for tableHTML to fix problems with render_tableHTML after the recent update of package shiny.

## Test environments
* local windows, R 3.2.0, R 3.3.0, R devel (also winbuilder on CRAN)
* ubuntu 12.04 LTS (on travis-ci), R 3.3.0

## R CMD check results
There were no ERRORs, WARNINGs

1 Note:

render_tableHTML : renderFunc: no visible global function definition
  for 'func'
Undefined global functions or variables:
  func

## Explanation

In my package I am assigning shiny::renderUI to render_tableHTML

In the new version of shiny renderUI is defined as:

```
function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        result <- func()
        if (is.null(result) || length(result) == 0) 
            return(NULL)
        processDeps(result, shinysession)
    }
    markRenderFunction(uiOutput, renderFunc, outputArgs = outputArgs)
}
```

The problem is on the 5th line with the function func(). func() is created in the current environment
from installExprFunction in the previous line (when renderUI is run) and it doesn't come from a separate package. I think
the checks are not able to assess that func gets created in installExprFunction and returns this NOTE. I am not sure whether
I can do anything to remove it completely, but render_tableHTML works as expected in all my examples.  


program define value_review 
args filenames

// Loop over each .dta file in the directory
foreach file in `filenames' {
// Load the dataset
use "`file'", clear
* Get the list of string variables
ds, has(type string)
local string_vars `r(varlist)'
* Get the list of numeric variables (which includes both integers and floats)
ds, has(type numeric)
local numeric_vars `r(varlist)'
* Separate integer and float variables
local int_vars
local float_vars
foreach var of local numeric_vars {
    quietly summarize `var'
    if r(max) == floor(r(max)) & r(min) == floor(r(min)) {
        local int_vars `int_vars' `var'
    }
    else {
        local float_vars `float_vars' `var'
    }
}
cap log close
log using "$rootdir/Build/Output/`filenames'_value_review", replace text

* Review string variables
if "`string_vars'" != "" {
    di "Review of String Variables"
    foreach var of local string_vars {
        di "Variable: `var'"
        tab `var', missing
        di ""
    }
}
* Review integer variables
if "`int_vars'" != "" {
    di "Review of Integer Variables"
    foreach var of local int_vars {
        di "Variable: `var'"
        summarize `var', detail
        di ""
    }
}
* Review float variables
if "`float_vars'" != "" {
    di "Review of Float Variables"
    foreach var of local float_vars {
        di "Variable: `var'"
        summarize `var', detail
        di ""
    }
}
log close
}
end


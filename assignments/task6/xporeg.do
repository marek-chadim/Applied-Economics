    clear all
	use penn_jae
	keep if tg ==0 | tg == 4
	rename tg D
	replace D = 1 if D == 4
	gen Y = ln(inuidur1)
	
	foreach x of varlist female-husd{
	foreach y of varlist female-husd {
	generate `x'X`y'=`x'*`y'
	}
	}
	
	vl set
	vl move (D) vlother
	vl substitute ifactors = i.vlcategorical
	display "$ifactors"

	qui: xporegress Y D, controls($ifactors) xfolds(5) resample(15) rseed(42) selection(plugin) vce(cluster abdt)
	etable

	dis e(k_controls_sel)
	dis e(controls_sel)

	qui: xporegress Y D, controls($ifactors) xfolds(5) resample(15) rseed(42) selection(cv) vce(cluster abdt)
	etable

    dis e(k_controls_sel)

	vl move (female-husd) vlother
	vl substitute ifactors2 = i.vlcategorical
	display "$ifactors2"


	qui: xporegress Y D , controls( (female-husd) $ifactors2) xfolds(5) resample(15) rseed(42) selection(plugin) vce(cluster abdt)
	etable

    dis e(k_controls_sel)


    dis e(controls_sel)



****************************************************
* FONTAGNE-STYLE BASELINE ESTIMATION
* Adapted to your thesis dataset
*
* Keeps only:
*   1. PPML with zeros included
*   2. PPML excluding zero trade flows

****************************************************

clear all
set more off
set maxvar 32000
set matsize 11000


****************************************************
* 0. LOAD YOUR MAIN DATASET
****************************************************

use "/Users/artemsokolov/Desktop/Диплом/Data/replication/final data for regressions/thesis_may_FINAL.dta", clear

drop if i_iso == j_iso

****************************************************
* 1. DEFINE VARIABLES
****************************************************

local y        value_1000usd
local tariff   ln_tariff
local hs6      k
local importer j_iso
local exporter i_iso
local year     t

local distvar  dist
local controls contig comlang_off comcol


****************************************************
* 2. BASIC PREPARATION
****************************************************

replace `y' = 0 if missing(`y')

capture confirm string variable `hs6'
if _rc {
    tostring `hs6', replace format(%06.0f)
}

capture drop l_dist
gen l_dist = ln(`distvar')

capture drop imp_year exp_year pair_id


****************************************************
* 3. SAVE LIST OF HS6 PRODUCTS
****************************************************

preserve

keep `hs6'
duplicates drop
sort `hs6'
save "hs6_product_list.dta", replace

restore


****************************************************
* 4. LOOP OVER HS6 PRODUCTS
****************************************************

use "hs6_product_list.dta", clear
levelsof `hs6', local(products)

tempfile all_results
save `all_results', emptyok replace

local total : word count `products'
local counter = 0

foreach x of local products {

    local ++counter
    di as text "Estimating HS6 product `x' (`counter'/`total')"

    use "/Users/artemsokolov/Desktop/Диплом/Data/replication/final data for regressions/thesis_may_FINAL.dta", clear
	drop if i_iso == j_iso

    replace `y' = 0 if missing(`y')

    capture confirm string variable `hs6'
    if _rc {
        tostring `hs6', replace format(%06.0f)
    }

    keep if `hs6' == "`x'"

    quietly count
    if r(N) == 0 continue

    capture drop l_dist
    gen l_dist = ln(`distvar')

    capture drop imp_year exp_year pair_id
    egen imp_year = group(`importer' `year')
    egen exp_year = group(`exporter' `year')
    egen pair_id  = group(`importer' `exporter')


    ************************************************
    * 4.1 FONTAGNE SAMPLE RESTRICTION
    * Drop exporters that never export this product
    ************************************************

    bysort `exporter': egen exporter_trade = total(`y')
    drop if exporter_trade == 0
    drop exporter_trade



    ************************************************
    * 4.2 INITIALIZE RESULT VARIABLES
    ************************************************

    gen sigma_ppml = .
    gen std_err_ppml = .
    gen sigma_ppml_dist = .
    gen std_err_ppml_dist = .

    gen sigma_ppml_nozero = .
    gen std_err_ppml_nozero = .


    ************************************************
    * 4.3 BASELINE PPML, ZEROS INCLUDED
    ************************************************

    capture noisily ppmlhdfe `y' `tariff' l_dist `controls', absorb(`exporter' `importer' `year') vce(robust)

    if _rc == 0 {
        capture replace sigma_ppml = _b[`tariff']
        capture replace std_err_ppml = _se[`tariff']

        capture replace sigma_ppml_dist = _b[l_dist]
        capture replace std_err_ppml_dist = _se[l_dist]
    }


    ************************************************
    * 4.4 PPML EXCLUDING ZERO TRADE FLOWS
    ************************************************

    capture noisily ppmlhdfe `y' `tariff' l_dist `controls' if `y' > 0, absorb(`exporter' `importer' `year') vce(robust)

    if _rc == 0 {
        capture replace sigma_ppml_nozero = _b[`tariff']
        capture replace std_err_ppml_nozero = _se[`tariff']
    }


    ************************************************
    * 4.5 COLLAPSE TO ONE ROW PER HS6 PRODUCT
    ************************************************

    gen sector = "`x'"
    gen obs = 1

    keep sector sigma_ppml std_err_ppml sigma_ppml_dist std_err_ppml_dist sigma_ppml_nozero std_err_ppml_nozero `y' obs

    collapse (mean) sigma_ppml std_err_ppml sigma_ppml_dist std_err_ppml_dist sigma_ppml_nozero std_err_ppml_nozero (sum) trade_value = `y' obs, by(sector)


    ************************************************
    * 4.6 T-STATISTICS
    ************************************************

    gen t_ppml = abs(sigma_ppml / std_err_ppml)
    gen t_ppml_dist = abs(sigma_ppml_dist / std_err_ppml_dist)
    gen t_ppml_nozero = abs(sigma_ppml_nozero / std_err_ppml_nozero)


    ************************************************
    * 4.7 SAVE PRODUCT RESULT AND APPEND
    ************************************************

    tempfile result_`x'
    save `result_`x'', replace

    use `all_results', clear
    append using `result_`x''
    save `all_results', replace
}


****************************************************
* 5. LOAD COMBINED PRODUCT-LEVEL RESULTS
****************************************************

use `all_results', clear

drop if missing(sector)

rename sector HS6


****************************************************
* 6. FONTAGNE 1% SIGNIFICANCE RULE
****************************************************

gen sigma_ppml_original = sigma_ppml
gen epsilon_original = 1 + sigma_ppml_original

replace sigma_ppml = 0 if t_ppml < 1.96
replace sigma_ppml_nozero = 0 if t_ppml_nozero < 1.96
replace sigma_ppml_dist = 0 if t_ppml_dist < 1.96


****************************************************
* 7. COMPUTE TRADE ELASTICITIES
****************************************************

gen epsilon_ppml = 1 + sigma_ppml
gen epsilon_ppml_nozero = 1 + sigma_ppml_nozero

replace epsilon_ppml = . if sigma_ppml == .
replace epsilon_ppml_nozero = . if sigma_ppml_nozero == .


****************************************************
* 8. CREATE FLAGS, AS IN FONTAGNE
****************************************************

gen zero = (sigma_ppml == 0 | t_ppml < 1.96)
replace zero = . if sigma_ppml == .

gen positive = (sigma_ppml > 0 & t_ppml >= 1.96 & sigma_ppml != .)

gen missing = (sigma_ppml == .)

gen positive_original = (sigma_ppml_original > 0 & sigma_ppml_original != .)


****************************************************
* 9. CREATE HS4 AND HS2
****************************************************

capture confirm string variable HS6
if _rc {
    tostring HS6, replace format(%06.0f)
}

gen HS4 = substr(HS6, 1, 4)
gen HS2 = substr(HS6, 1, 2)


****************************************************
* 10. FONTAGNE HS4 / HS2 FALLBACK PROCEDURE
****************************************************

gen epsilon = epsilon_ppml if epsilon_ppml < 0

bysort HS4: egen avg_epsilon_HS4 = mean(epsilon)
bysort HS2: egen avg_epsilon_HS2 = mean(epsilon)

replace epsilon = avg_epsilon_HS4 if zero == 1 & avg_epsilon_HS4 != .
replace epsilon = avg_epsilon_HS4 if positive == 1 & avg_epsilon_HS4 != .
replace epsilon = avg_epsilon_HS4 if missing == 1 & avg_epsilon_HS4 != .
replace epsilon = avg_epsilon_HS4 if sigma_ppml < 0 & epsilon_ppml > 0 & avg_epsilon_HS4 != .

replace epsilon = avg_epsilon_HS2 if zero == 1 & avg_epsilon_HS4 == . & avg_epsilon_HS2 != .
replace epsilon = avg_epsilon_HS2 if positive == 1 & avg_epsilon_HS4 == . & avg_epsilon_HS2 != .
replace epsilon = avg_epsilon_HS2 if missing == 1 & avg_epsilon_HS4 == . & avg_epsilon_HS2 != .
replace epsilon = avg_epsilon_HS2 if sigma_ppml < 0 & epsilon_ppml > 0 & avg_epsilon_HS4 == . & avg_epsilon_HS2 != .


****************************************************
* 11. ORIGINAL POINT-ESTIMATE ELASTICITY
****************************************************

gen epsilon_pt = epsilon_original
gen positive_pt = (epsilon_pt > 0 & epsilon_pt != .)
replace epsilon_pt = . if epsilon_pt > 0


****************************************************
* 12. SAVE FULL PRODUCT-LEVEL RESULTS
****************************************************

save "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/elasticity_baseline_fontagne_style.dta", replace

export excel using "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/elasticity_baseline_fontagne_style.xlsx", firstrow(variables) replace


****************************************************
* 13. CREATE PUBLICATION-STYLE ELASTICITY DATASET
****************************************************

preserve

keep HS6 epsilon zero positive missing epsilon_pt positive_pt

label var HS6 "HS 6-digit product category"
label var epsilon "Trade elasticity based on 1% significant tariff elasticity"
label var zero "Dummy equal to 1 when tariff elasticity is not statistically different from 0 at 1%"
label var positive "Dummy equal to 1 when tariff elasticity is positive and statistically significant at 1%"
label var missing "Dummy equal to 1 when tariff elasticity could not be estimated"
label var epsilon_pt "Trade elasticity based on point estimate"
label var positive_pt "Dummy equal to 1 when point-estimate trade elasticity is positive"

save "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/elasticity_publication_style.dta", replace

export excel using "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/elasticity_publication_style.xlsx", firstrow(variables) replace

restore


****************************************************
* 14. CREATE TABLE 5 STYLE HS SECTION STATISTICS
****************************************************

gen hs2_num = real(HS2)

gen hs_section = .
replace hs_section = 1  if inrange(hs2_num, 1, 5)
replace hs_section = 2  if inrange(hs2_num, 6, 14)
replace hs_section = 3  if hs2_num == 15
replace hs_section = 4  if inrange(hs2_num, 16, 24)
replace hs_section = 5  if inrange(hs2_num, 25, 27)
replace hs_section = 6  if inrange(hs2_num, 28, 38)
replace hs_section = 7  if inrange(hs2_num, 39, 40)
replace hs_section = 8  if inrange(hs2_num, 41, 43)
replace hs_section = 9  if inrange(hs2_num, 44, 46)
replace hs_section = 10 if inrange(hs2_num, 47, 49)
replace hs_section = 11 if inrange(hs2_num, 50, 63)
replace hs_section = 12 if inrange(hs2_num, 64, 67)
replace hs_section = 13 if inrange(hs2_num, 68, 70)
replace hs_section = 14 if hs2_num == 71
replace hs_section = 15 if inrange(hs2_num, 72, 83)
replace hs_section = 16 if inrange(hs2_num, 84, 85)
replace hs_section = 17 if inrange(hs2_num, 86, 89)
replace hs_section = 18 if inrange(hs2_num, 90, 92)
replace hs_section = 19 if hs2_num == 93
replace hs_section = 20 if inrange(hs2_num, 94, 96)
replace hs_section = 21 if hs2_num == 97

gen description = ""
replace description = "Live Animals and Animal Products" if hs_section == 1
replace description = "Vegetable Products" if hs_section == 2
replace description = "Animal or Vegetable Fats and Oils" if hs_section == 3
replace description = "Prepared Foodstuffs, Beverages and Tobacco" if hs_section == 4
replace description = "Mineral Products" if hs_section == 5
replace description = "Products of Chemical Industries" if hs_section == 6
replace description = "Plastics and Rubber" if hs_section == 7
replace description = "Raw Hides, Skins, Leather" if hs_section == 8
replace description = "Wood, Cork and Articles" if hs_section == 9
replace description = "Pulp, Paper and Printed Products" if hs_section == 10
replace description = "Textiles and Textile Articles" if hs_section == 11
replace description = "Footwear, Headgear, Umbrellas" if hs_section == 12
replace description = "Stone, Plaster, Ceramic and Glass" if hs_section == 13
replace description = "Pearls, Precious Stones and Metals" if hs_section == 14
replace description = "Base Metals and Articles" if hs_section == 15
replace description = "Machinery and Electrical Equipment" if hs_section == 16
replace description = "Vehicles, Aircraft and Transport Equipment" if hs_section == 17
replace description = "Optical, Photographic and Precision Instruments" if hs_section == 18
replace description = "Arms and Ammunition" if hs_section == 19
replace description = "Miscellaneous" if hs_section == 20
replace description = "Works of Art" if hs_section == 21

gen one = 1

preserve

gen epsilon_table5 = epsilon_original
replace epsilon_table5 = . if epsilon_table5 > 0

collapse (mean) Average = epsilon_table5 (sd) Std_Dev = epsilon_table5 (min) Min = epsilon_table5 (sum) No_HS6 = one (count) No_HS6_nonmissing = epsilon_table5, by(hs_section description)

sort hs_section

order hs_section description Average Std_Dev Min No_HS6 No_HS6_nonmissing

export excel using "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/Table5_fontagne_style.xlsx", firstrow(variables) replace

restore


****************************************************
* 15. SAVE FINAL DATASET AGAIN
****************************************************

save "/Users/artemsokolov/Desktop/Диплом/Data/replication/april29/elasticity_RESULTS_april_29.dta", replace

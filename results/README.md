# **Final set of CamtrapSimulation results** 

This directory contains the final set of CamtrapSimulation results.

All the paths have been simulated using body mass as the input parameter.

All correct paths and results are in the final_results folder.

All previous paths and results are in the prev_results folder.
___

## **final_results folder**

### **Paths**

note: paths are all final versions to be used in results

**Unimodal paths: paths_uni**

Paths generated on 08Jul22 with body mass as input parameter and corrected scaling relationships. 


**Bimodal paths: paths_bi**

Paths generated on 27Jul22 with body mass as input parameter and artificially-created bimodal speed distributions by just using unimodal speed distribution scaling, reducing mean logspeed by 30% for the slow, more tortuous speeds and increaseing by 30% for the faster, less tortuous speeds.



## **Simulation sequence data and plotting variables**

note: all the seq_dats are correct but only done for the first 5 of each
all the plotting variables are only done for the first 5 of each and also don't contain numbers or speeds of zeros, and the estimated speeds when adding back in singles & zeros actually only have singles added back in

--> so basically need to re-run everything using all 20 and including zeros too



**uni_hz_noscaling**

Unimodal paths. Hazard rate function parameterized using all Panama species used - no body mass scaling of detection distance probability from CT.

**uni_hz_scaling**

Unimodal paths. Hazard rate function with body mass scaling trends of its parameters (scaling trends derived using Panama data). 

**bi_hz_scaling**

Same as uni_hz_effdist except using bimodal paths instead of unimodal. 


## **Plots**

Plots for each of the three scenarios in sequence data and plotting variables.

___

# **prev_results folder**

## **bi_palencia**

Previous paths, seq_dats and plotting variables using paths generated on 23Jul22 with body mass as input parameter and scaling relationships derived using Pablo's data - but speeds came out way too high so instead artificially create bi-modal distribution myself.


## **30Jun22_1727**

Paths generated using body mass, but mean speeds were off because there were mistakes in body mass - speed scaling equations.

## **08Jul22_incorrect_seqdats**

**no_log_mix**

Using correct unimodal paths. Using detection probability function of hazard rate + logistic mix parameterized using only large species in RP & India data. Also contains plots for these

**w_log_mix**

Using correct unimodal paths. Using the following detection probability functions:
- small species: hz + logistic mix, parameterized using small spp in RP & India data only
- large species: hz + logistic mix: parameterized using large spp in RP & India data only

## **sp_results_correctSD**

Paths generated using speed parameter as input parameter. Next improvement was to use body mass instead though.

## **sp_results_wrongSD**

Paths generated using speed parameter as input parameter. Wrong SD though - fixed in next set generated in sp_results_correctSD.


# runs from impact analysis

clear
use "/Users/ericgreen/Box Sync/SENSITIVE Folder epg4/Repositories/github/pmd/data and replication files/master/pmd1/output/stata/stata.dta"
set more off


rename b_care_marital_mar_inrel_cohab b_care_marcohab
rename b_care_tc_relation_bioparent b_bioparent
rename b_care_religion_christian b_christian
rename b_care_work_inc_mo_all_99 b_incall99



foreach v in p_harshdiscipline_a p_pbm_a c_posint_a p_sdq_conduct {
	egen `v'z = std(`v')

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.1)
	estimates store q1

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.2)
	estimates store q2

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.3)
	estimates store q3

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.4)
	estimates store q4

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.5)
	estimates store q5

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.6)
	estimates store q6

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.7)
	estimates store q7

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.8)
	estimates store q8

	xi: qreg `v'z b_treatment i.community_id b_care_female b_care_marcohab b_christian b_incall99 b_care_work_hoursinweek b_care_num_fam_inhh b_care_num_dep_u18 b_bioparent b_care_tc_age b_care_tc_female b_care_sdq_conductprob, quantile(.9)
	estimates store q9

	esttab q1 q2 q3 q4 q5 q6 q7 q8 q9 using "/Users/ericgreen/Box Sync/SENSITIVE Folder epg4/Repositories/github/pmd/data and replication files/master/pmd1/output/stata/output/stataout-`v'.csv", cells("b se t p ci") wide replace plain

}

* grqreg, cons ci ols olsci reps(40) name(tmp1, replace)

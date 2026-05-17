ssri_criteria <- list(
    include = paste(
        c(
            "- Randomized controlled trials and non-randomized comparative cohort studies.",
            "- Included COVID-19 inpatients and/or outpatients of any age.",
            "- Compared selective serotonin reuptake inhibitors, mainly fluvoxamine, fluoxetine, or citalopram/escitalopram, against standard of care or placebo.",
            "- Reported at least one of the following outcomes: all-cause mortality, hospitalization, composite emergency room visits or hospitalization, hypoxemia, supplemental oxygen requirement, ventilator support, or serious adverse events."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies assessing patients with past or ongoing SSRI prescriptions at the time of COVID-19 diagnosis."
        ),
        collapse = "\n"
    )
)

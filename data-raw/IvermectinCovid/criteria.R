ivm_criteria <- list(
    include = paste(
        c(
            "- Randomized controlled trials and non-randomized comparative cohort studies.",
            "- Compared ivermectin against standard of care or a control/placebo group.",
            "- Included adult (age >= 18 years) COVID-19 inpatients and/or outpatients.",
            "- Reported at least one of the following outcomes: time to viral clearance, duration of hospitalization, mortality incidence, incidence of progression to mechanical ventilation, all-cause adverse events, or investigator-defined serious adverse events."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies that used ivermectin for prophylaxis of COVID-19.",
            "- Non-peer-reviewed articles."
        ),
        collapse = "\n"
    )
)

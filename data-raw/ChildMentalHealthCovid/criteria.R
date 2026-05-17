child_criteria <- list(
    include = paste(
        c(
            "- Primary cross-sectional or longitudinal studies conducted after January 2020, or studies reporting time points after January 2020.",
            "- Included children and adolescents <= 18 years of age or students enrolled in primary or secondary educational institutions.",
            "- Reported the prevalence of depressive symptoms, anxiety symptoms, and/or sleep disturbances using self-reported instruments or clinical interviews."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies specifically targeting children with special needs or pre-existing mental health disorders."
        ),
        collapse = "\n"
    )
)

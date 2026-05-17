lpvr_criteria <- list(
    include = paste(
        c(
            "- Randomized controlled trials and comparative non-randomized observational studies.",
            "- Compared lopinavir-ritonavir with standard of care, or compared lopinavir-ritonavir plus adjuvant therapies against the same adjuvant therapies alone.",
            "- Included laboratory-confirmed, hospitalized COVID-19 patients."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies that used different adjuvant therapies in the intervention and control arms."
        ),
        collapse = "\n"
    )
)

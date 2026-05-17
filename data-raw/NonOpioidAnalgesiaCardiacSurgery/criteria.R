noa_criteria <- list(
    include = paste(
        c(
            "- Randomized controlled trials.",
            "- Compared non-opioid analgesics against active non-opioid comparators, placebo, or no additional treatment as adjuvant therapy with standard analgesic management.",
            "- Included adult patients (age >= 18 years) undergoing cardiac surgery.",
            "- Reported at least one of the following outcomes: resting postoperative pain scores at 24 hours, 24-hour postoperative opioid consumption, ICU length of stay, duration of mechanical ventilation, myocardial infarction, delirium, nausea, or vomiting."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies enrolling patients undergoing minimally invasive surgery.",
            "- Studies assessing nerve blocks or local anesthetics."
        ),
        collapse = "\n"
    )
)

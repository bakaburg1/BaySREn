giesen_criteria <- list(
    # Define the explicit inclusion scope from the review protocol.
    include = c(
        "Population: HIV-infected children aged 0 to 15 years.",
        "Topic relevance: Studies must address the etiology of community-acquired pneumonia.",
        "Geography: Studies must assess Sub-Saharan African countries.",
        "Language: Studies published in English, French, German, Portuguese, or Spanish.",
        "Study designs: Prospective research studies, antemortem studies, postmortem studies, and randomized controlled trials.",
        "Sample size: Studies must include at least 20 cases."
    ),
    # Mirror the explicit exclusion criteria listed in the review material.
    exclude = c(
        "Topic relevance: Studies not addressing community-acquired pneumonia etiology.",
        "Population: Studies not addressing HIV-infected children.",
        "Population: Studies focused on children older than 15 years.",
        "Geography: Studies not addressing Sub-Saharan African countries.",
        "Sample size: Studies including fewer than 20 cases.",
        "Language: Studies published in languages other than English, French, German, Portuguese, or Spanish.",
        "Study designs: Retrospective studies, systematic reviews, meta-analyses, and clinical practice guidelines."
    )
)

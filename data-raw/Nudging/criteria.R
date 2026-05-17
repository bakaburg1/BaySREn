nudging_criteria <- list(
    include = paste(
        c(
            "- Source review: Nagtegaal et al. (2019), Nudging healthcare professionals towards evidence-based medicine: A systematic scoping review.",
            "- Include empirical studies on nudging interventions aimed at healthcare professionals and evidence-based medicine behavior.",
            "- The dataset is treated as usable because the ASReview metadata marks both final inclusions and title/abstract inclusions as available, and the retained labels match the review's reported included article count."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Records outside the healthcare-professional nudging scope.",
            "- Records not addressing evidence-based medicine behavior or relevant professional decision-making.",
            "- Records not retained as included by the review screening/coding workflow."
        ),
        collapse = "\n"
    )
)

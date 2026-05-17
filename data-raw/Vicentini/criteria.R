vicentini_criteria <- list(
    # Define the inclusion scope around LTCF AMS implementation barriers.
    include = c(
        "Population: Adult residents of long-term care facilities, nursing homes, and similar long-term care institutions for the elderly.",
        "Topic relevance: Studies must address barriers to the implementation of antimicrobial stewardship programmes in long-term care facilities.",
        "Programme scope: Eligible interventions are full antimicrobial stewardship programmes, defined in the paper as interventions fulfilling at least four of the seven CDC Core Elements of Antibiotic Stewardship for Nursing Homes.",
        "Context: Long-term care facilities implementing antimicrobial stewardship programmes.",
        "Study designs: Qualitative studies, surveys, randomized controlled trials, quasi-experimental studies, and systematic or structured reviews."
    ),
    # Mirror the exclusion logic reported in the paper.
    exclude = c(
        "Setting: Studies conducted outside long-term care facilities, nursing homes, or comparable long-term care institutions for the elderly.",
        "Topic relevance: Studies not focused on antimicrobial stewardship programmes.",
        "Topic relevance: Studies not addressing implementation barriers.",
        "Publication type: Narrative reviews, editorials, commentaries, conference abstracts, and unpublished articles.",
        "Availability: Articles for which the full text was not available."
    )
)

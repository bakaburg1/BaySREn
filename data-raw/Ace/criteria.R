ace_criteria <- list(
    include = paste(
        c(
            "- Source label is the OHSU gold-standard `Abstract Triage Status` for the ACE inhibitor drug-class review topic.",
            "- Code `I` means the citation passed abstract triage in the drug-class review source.",
            "- Topic: angiotensin-converting enzyme inhibitors as one of the drug-class systematic review topics in the Cohen benchmark collection."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Records not coded as `I` in the OHSU `Abstract Triage Status` field.",
            "- OHSU triage codes distinguish several exclusion causes, including off-topic/non-drug, basic science, duplicate, and non-English records."
        ),
        collapse = "\n"
    )
)

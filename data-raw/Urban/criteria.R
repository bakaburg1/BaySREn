urban_criteria <- list(
    include = paste(
        c(
            "- Publication years: 2016 to 2022.",
            "- Language: English-language publications; missing language tags in the RIS export were retained unless a non-English language was explicit.",
            "- Source type: Peer-reviewed journal articles, reviews, and editorials retrieved from Scopus and Web of Science.",
            "- Step 1 topic screen: Records addressing sustainability, cities, planning, and nature, with a joker override for clearly relevant keyword combinations.",
            "- Step 2 abstract screen: Empirical studies on cities, planning, and nature in countries of the European Union, including the United Kingdom."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Records outside the 2016 to 2022 publication window.",
            "- Records not dealing with sustainability or the spatial context of cities.",
            "- Records without a planning focus or without any nature component.",
            "- Before full-text screening, non-empirical studies and studies outside the EU/UK scope.",
            "- Full-text eligibility decisions are not represented in this package import because the package collapses title/keyword and abstract screening into a single pre-fulltext label."
        ),
        collapse = "\n"
    )
)

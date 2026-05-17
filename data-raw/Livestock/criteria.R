livestock_criteria <- list(
    include = paste(
        c(
            "- Publication years: 2011 to 2025.",
            "- Topic relevance: Studies on livestock ontologies, ontology-based livestock health management, disease surveillance, infectious disease monitoring, or related livestock health data management challenges.",
            "- Population and scope: Farm animals, especially cattle, poultry, and pigs, within agriculture or animal agriculture contexts.",
            "- Sources searched: PubMed, IEEE Xplore, and Google Scholar.",
            "- Evidence type: Peer-reviewed articles and related literature retained in the ASReview export."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Records outside the 2011 to 2025 publication window.",
            "- Records not related to livestock or farm-animal health management.",
            "- Records without an ontology, ontology-based, disease-surveillance, or livestock-data-management focus.",
            "- Records outside agriculture or animal-agriculture contexts."
        ),
        collapse = "\n"
    )
)

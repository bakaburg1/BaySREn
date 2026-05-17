gastaldi_criteria_revised <- list(
    # Define the amended inclusion scope around manual-ready IPC content.
    include = paste(
        c(
            "- Language: Publications in English.",
            paste(
                "- Topic relevance: Documents whose main focus is",
                "concrete, facility-level IPC manual/handbook content,",
                "structured IPC guidance, standard operating procedures,",
                "checklists, bundles, or programme elements that could",
                "directly inform the table of contents or contents of an",
                "institutional IPC manual."
            ),
            paste(
                "- Topic relevance: Broad IPC programmes, policies,",
                "surveillance systems, preparedness activities, or WHO",
                "IPC Core Components are eligible only when they provide",
                "explicit, structured, operational content that could be",
                "incorporated into a facility IPC manual."
            ),
            paste(
                "- Type of evidence: All study/document types",
                "(systematic reviews, cross-sectional studies, research",
                "reports, guidelines, technical documents from",
                "WHO/ECDC/national sources, expert opinion, relevant",
                "grey literature)."
            ),
            paste(
                "- Setting/scope: Evidence applicable to healthcare",
                "facilities in any income setting (HICs and LMICs)."
            ),
            paste(
                "- Full-text pertinence: Full texts must contribute",
                "usable sections, headings, procedures, or operational",
                "elements for constructing an IPC manual/framework."
            )
        ),
        collapse = "\n"
    ),
    # Exclude IPC-adjacent literature that lacks manual-ready content.
    exclude = paste(
        c(
            "- Language: Publications in languages other than English.",
            paste(
                "- Topic relevance: Documents discussing IPC in general,",
                "WHO IPC Core Components, HAI surveillance, stewardship,",
                "preparedness, or isolated IPC measures without direct,",
                "structured content for a facility IPC manual/handbook."
            ),
            paste(
                "- Topic relevance: Studies focused mainly on",
                "implementation outcomes, compliance,",
                "knowledge/attitudes, epidemiology, or technology",
                "performance unless they explicitly describe",
                "manual-ready IPC procedures or operational guidance."
            ),
            paste(
                "- Type of evidence: Documents without publication",
                "references or discussion-only material with no",
                "sourceable content."
            ),
            paste(
                "- Setting/scope: Material on infection control not",
                "applicable/transferable to healthcare facilities",
                "(e.g. purely community, environmental, veterinary, if",
                "not clearly adaptable)."
            ),
            "- Duplicates: Duplicate records identified during the search.",
            paste(
                "- Full-text pertinence: Full texts that are",
                "IPC-relevant but do not provide usable IPC content for",
                "an IPC manual/framework."
            )
        ),
        collapse = "\n"
    )
)

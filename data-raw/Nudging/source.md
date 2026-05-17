# Nudging Source Notes

- Local object: `nudging`
- Local raw file: `data-raw/Nudging/data.csv`
- Builder: `data-raw/Nudging/nudging.R`
- Upstream file used by the builder: `https://raw.githubusercontent.com/asreview/systematic-review-datasets/metadata-v1-final/datasets/Nagtegaal_2019/output/Nagtegaal_2019.csv`
- Source review: Nagtegaal, R.; Tummers, L.; Noordegraaf, M.; Bekkers, V. `Nudging healthcare professionals towards evidence-based medicine: A systematic scoping review`, Journal of Behavioral Public Administration, 2019, DOI `10.30636/jbpa.22.71`.
- Source evidence: ASReview/SYNERGY metadata marks `title_abstract_inclusions = TRUE`; the public review and dataset record provide the review context and criteria.
- Provenance judgment: usable, but weaker than ACE because the public data page is a companion code set rather than a clearly phase-labeled triage export.

## Import Notes

- The local ASReview-derived CSV contains `1847` records and `100` positives.
- Some online ASReview metadata reports `2019` records and `101` positives for a later or broader version; this import preserves the exact CSV already used by the package.

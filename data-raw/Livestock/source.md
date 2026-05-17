# Livestock

## Data Source

1. Zenodo dataset DOI: `10.5281/zenodo.15013184`.

2. Local source files:
   1. `asreview_dataset_all_a-review-on-the-role-of-ontologies-in-modern-agriculture.xlsx`.
   2. `Supplementary_Information.docx`.

## Review Paper

1. Title: `A systematic review on the role of livestock ontologies in animal health management and disease surveillance: A PRISMA 2020 and AI-assisted screening approach`.

2. Authors: Saba Noor; Jeroen Degroote; Gerdien van Schaik; Bart Pardon; Celine Faverjon; Camille Delavenne; Miel Hostens.

3. Journal: `Smart Agricultural Technology`.

4. Year: 2026.

5. DOI: `10.1016/j.atech.2026.101977`.

## Imported Label

1. Packaged object: `livestock`.

2. Base pool used for import: the full ASReview workbook export.

3. Package label meaning: the abstract-screening `included` label exported by ASReview after enforcing the publication-year restriction stated in the supplementary material.

4. Positive label count after import corrections: 111.

## Criteria

1. Criteria source: supplementary material.

2. Local criteria file: `criteria.R`.

3. The supplementary material states that the study focused on research published between 2011 and 2025.

4. The search targeted livestock health ontologies, disease surveillance, ontology-based approaches, precision livestock farming, livestock management systems, and related data-management challenges in agriculture.

5. Sources searched: PubMed, IEEE Xplore, and Google Scholar.

## Import Notes

1. The Zenodo supplementary material reports 286 screened records and 115 relevant records in the ASReview export.

2. The published review paper reports 286 screened records but only 100 final included studies.

3. The ASReview workbook still contains 4 included records from 2008 to 2010, outside the supplementary material's stated 2011 to 2025 restriction. Those records were removed during import.

4. After applying the year restriction, the packaged dataset contains 282 records and 111 positives.

5. The package keeps the abstract-screening labels from ASReview, not the final review inclusion set from the 2026 paper.

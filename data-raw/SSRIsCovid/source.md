# Selective serotonin reuptake inhibitors for COVID-19

## Dataset Source

1. Local dataset file: `data.csv`.

2. Distribution source: Mendeley Data dataset `Automated Paper Screening for Clinical Reviews Using Large Language Models`.

3. Distribution DOI: `10.17632/np79tmhkh5.1`.

4. Contributor names listed on the public Mendeley dataset page: Eddie Guo; Mehul Gupta; Jiawen Deng; Ye-Jean Park; Mike Paget; Christopher Naugler.

5. The JMIR benchmark paper PDF is stored in `references/JMIR_LLM_screening/jmir_llm_screening_paper.pdf`.

6. The source bundle's `dataset_info.csv` is stored in `references/JMIR_LLM_screening/dataset_info.csv`.

7. The Mendeley bundle also contained `llm_clin_rev_data.csv` and `df_with_decisions.csv`. Those files were not imported because they are not one of the six reviews documented in `dataset_info.csv`, no first-party criteria block is bundled for them, and the topical Nature Medicine LLM review identified later does not match their 2,893 records and 23 positives.

## Original Review

1. Review title: `Efficacy and safety of selective serotonin reuptake inhibitors in COVID-19 management: a systematic review and meta-analysis`.

2. Authors: Jiawen Deng; Daniel Rayner; Harikrishnaa B. Ramaraju; Umaima Abbas; Cristian Garcia; Kiyan Heybati; Fangwen Zhou; Emma Huang; Ye-Jean Park; Myron Moskalyk.

3. Journal: `Clinical Microbiology and Infection`.

4. Year: 2023.

5. DOI: `10.1016/j.cmi.2023.01.010`.

6. Review PDF: not stored yet. Direct PDF retrieval from the open article endpoints returned an HTML download-gating page in this environment; retry manually from the DOI or PMC landing page if a local PDF copy is required.

## Criteria

1. Criteria source: `references/JMIR_LLM_screening/dataset_info.csv`.

2. Local criteria file: `criteria.R`.

## Local Counts

1. Rows: 4,018.

2. Included records: 29.

## Notes

1. Labels are taken from the JMIR/Mendeley benchmark distribution, not re-derived from the review PDF.

2. `dataset_info.csv` reports that the number may not match the publication due to uncaught duplicate entries.

3. `dataset_info.csv` lists 4,019 abstracts, while the local downloadable CSV contains 4,018 rows.

# Non-opioid analgesia in adult cardiac surgery

## Dataset Source

1. Local dataset file: `data.csv`.

2. Distribution source: Mendeley Data dataset `Automated Paper Screening for Clinical Reviews Using Large Language Models`.

3. Distribution DOI: `10.17632/np79tmhkh5.1`.

4. Contributor names listed on the public Mendeley dataset page: Eddie Guo; Mehul Gupta; Jiawen Deng; Ye-Jean Park; Mike Paget; Christopher Naugler.

5. The JMIR benchmark paper PDF is stored in `references/JMIR_LLM_screening/jmir_llm_screening_paper.pdf`.

6. The source bundle's `dataset_info.csv` is stored in `references/JMIR_LLM_screening/dataset_info.csv`.

7. The Mendeley bundle also contained `llm_clin_rev_data.csv` and `df_with_decisions.csv`. Those files were not imported because they are not one of the six reviews documented in `dataset_info.csv`, no first-party criteria block is bundled for them, and the topical Nature Medicine LLM review identified later does not match their 2,893 records and 23 positives.

## Original Review

1. Review title: `Comparative Efficacy of Adjuvant Nonopioid Analgesia in Adult Cardiac Surgical Patients: A Network Meta-Analysis`.

2. Authors: Kiyan Heybati; Fangwen Zhou; Matthew Joseph Lynn; Jiawen Deng; Saif Ali; Wenteng Hou; Shayan Heybati; Kosta Tzanis; Magnus Krever; Rafay Mughal; Harish Ramakrishna.

3. Journal: `Journal of Cardiothoracic and Vascular Anesthesia`.

4. Year: 2023.

5. DOI: `10.1053/j.jvca.2023.03.018`.

6. Review PDF: not stored yet. The publisher PDF endpoint returned HTTP 403 in this environment; retry manually from the DOI landing page if a local PDF copy is required.

## Criteria

1. Criteria source: `references/JMIR_LLM_screening/dataset_info.csv`.

2. Local criteria file: `criteria.R`.

## Local Counts

1. Rows: 15,125.

2. Included records: 354.

## Notes

1. Labels are taken from the JMIR/Mendeley benchmark distribution, not re-derived from the review PDF.

2. No dataset-specific count caveat is listed in `dataset_info.csv`.

3. `dataset_info.csv` lists 15,129 abstracts, while the local downloadable CSV contains 15,125 rows.

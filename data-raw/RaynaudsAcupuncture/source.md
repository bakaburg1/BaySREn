# Acupuncture for Raynaud's syndrome

## Dataset Source

1. Local dataset file: `data.csv`.

2. Distribution source: Mendeley Data dataset `Automated Paper Screening for Clinical Reviews Using Large Language Models`.

3. Distribution DOI: `10.17632/np79tmhkh5.1`.

4. Contributor names listed on the public Mendeley dataset page: Eddie Guo; Mehul Gupta; Jiawen Deng; Ye-Jean Park; Mike Paget; Christopher Naugler.

5. The JMIR benchmark paper PDF is stored in `references/JMIR_LLM_screening/jmir_llm_screening_paper.pdf`.

6. The source bundle's `dataset_info.csv` is stored in `references/JMIR_LLM_screening/dataset_info.csv`.

7. The Mendeley bundle also contained `llm_clin_rev_data.csv` and `df_with_decisions.csv`. Those files were not imported because they are not one of the six reviews documented in `dataset_info.csv`, no first-party criteria block is bundled for them, and the topical Nature Medicine LLM review identified later does not match their 2,893 records and 23 positives.

## Original Review

1. Review title: `The use of acupuncture in patients with Raynaud's syndrome: A systematic review and meta-analysis of randomized controlled trials`.

2. Authors: Fangwen Zhou; Emma Huang; Elena Zheng; Jiawen Deng.

3. Journal: `Acupuncture in Medicine`.

4. Year: 2023.

5. DOI: `10.1177/09645284221076504`.

6. Review PDF: not stored yet. The publisher PDF endpoint returned HTTP 403 in this environment; retry manually from the DOI landing page if a local PDF copy is required.

## Criteria

1. Criteria source: `references/JMIR_LLM_screening/dataset_info.csv`.

2. Local criteria file: `criteria.R`.

## Local Counts

1. Rows: 948.

2. Included records: 6.

## Notes

1. Labels are taken from the JMIR/Mendeley benchmark distribution, not re-derived from the review PDF.

2. `dataset_info.csv` reports that search results from Chinese databases and some malformed old search results were removed, so the numbers may not match the publication.

3. `dataset_info.csv` lists 949 abstracts, while the local downloadable CSV contains 948 rows.

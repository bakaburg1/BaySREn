# Lopinavir-ritonavir for hospitalized COVID-19

## Dataset Source

1. Local dataset file: `data.csv`.

2. Distribution source: Mendeley Data dataset `Automated Paper Screening for Clinical Reviews Using Large Language Models`.

3. Distribution DOI: `10.17632/np79tmhkh5.1`.

4. Contributor names listed on the public Mendeley dataset page: Eddie Guo; Mehul Gupta; Jiawen Deng; Ye-Jean Park; Mike Paget; Christopher Naugler.

5. The JMIR benchmark paper PDF is stored in `references/JMIR_LLM_screening/jmir_llm_screening_paper.pdf`.

6. The source bundle's `dataset_info.csv` is stored in `references/JMIR_LLM_screening/dataset_info.csv`.

7. The Mendeley bundle also contained `llm_clin_rev_data.csv` and `df_with_decisions.csv`. Those files were not imported because they are not one of the six reviews documented in `dataset_info.csv`, no first-party criteria block is bundled for them, and the topical Nature Medicine LLM review identified later does not match their 2,893 records and 23 positives.

## Original Review

1. Review title: `Efficacy of Lopinavir-Ritonavir Combination Therapy for the Treatment of Hospitalized COVID-19 Patients: A Meta-Analysis`.

2. Authors: Jiawen Deng; Fangwen Zhou; Wenteng Hou; Kiyan Heybati; Saif Ali; Oswin Chang; Zachary Silver; Thanansayan Dhivagaran; Harikrishnaa Ba Ramaraju; Chi Yi Wong; Qi Kang Zuo; Elizabeth Lapshina; Madeline Mellett.

3. Journal: `Future Virology`.

4. Year: 2022.

5. DOI: `10.2217/fvl-2021-0066`.

6. Review PDF: not stored yet. Direct PDF retrieval from the open article endpoints returned an HTML download-gating page in this environment; retry manually from the DOI or PMC landing page if a local PDF copy is required.

## Criteria

1. Criteria source: `references/JMIR_LLM_screening/dataset_info.csv`.

2. Local criteria file: `criteria.R`.

## Local Counts

1. Rows: 1,547.

2. Included records: 91.

## Notes

1. Labels are taken from the JMIR/Mendeley benchmark distribution, not re-derived from the review PDF.

2. `dataset_info.csv` reports that search results from Chinese databases were screened separately and are not included in this English dataset, so the numbers may not match the publication.

# Nature Medicine LLM-assisted benchmark

## Data Source

1. GitHub repository: `https://github.com/nyuolab/llms-in-clinical-medicine-systematic-review`.

2. Local source files:
   1. `deduped_and_processed_studies-GPT-5r-high.csv`.
   2. `compiled_deduped_with_abstracts.csv`.
   3. `screening_instructions.txt`.

## Review Paper

1. Title: `LLM-assisted systematic review of large language models in clinical medicine`.

2. Authors: Sully F. Chen; Anton Alyakin; Andreas Seas; Eunice Yang; Joanne J. Choi; Jin Vivian Lee; Amelia L. Chen; Pranav I. Warman; Rochelle T. Bitolas; Robert J. Steele; Daniel A. Alber; Eric K. Oermann.

3. Journal: `Nature Medicine`.

4. Year: 2026.

5. DOI: `10.1038/s41591-026-04229-5`.

## Imported Label

1. Packaged object: `nature_llm`.

2. Label meaning: GPT-5 title/abstract screening decision produced by the review pipeline.

3. Imported counts from the downloadable CSV: 12,896 records and 4,609 positives.

## Criteria

1. Criteria source: `screening_instructions.txt`.

2. Local criteria file: `criteria.R`.

3. Include studies evaluating a generative LLM in healthcare or medicine.

4. Include clinical evaluations such as board exams, triage, patient-question answering, diagnosis, and decision support.

5. Exclude non-LLM AI models, non-clinical LLM tasks such as abstract writing or data structuring, reviews/reports/surveys/editorials, and technical model architecture or training studies without clinical application.

## Import Notes

1. This is intentionally not a human-labeled benchmark. It is imported as a benchmark of an expensive AI screening method.

2. The paper reports 12,894 deduplicated studies, 4,609 included studies, and 8,285 excluded studies.

3. The downloadable GPT-5 screening CSV contains 12,896 records, 4,609 positives, and 8,287 negatives.

4. The package preserves the downloadable CSV counts and documents the two-record discrepancy.

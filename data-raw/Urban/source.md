# Urban

## Data Source

1. Zenodo dataset DOI: `10.5281/zenodo.8359407`.

2. Local source files:
   1. `B_01_recordsscreened-title-keywords.ris`.
   2. `B_05_screeningdecisions-overview.xlsx`.

## Review Paper

1. Title: `Planning cities with nature for sustainability transformations - a systematic review`.

2. Authors: Philip Harms; Maeve Hofer; Martina Artmann.

3. Journal: `Urban Transformations`.

4. Year: 2024.

5. DOI: `10.1186/s42854-024-00066-2`.

## Imported Label

1. Packaged object: `urban`.

2. Base pool used for import: 1,557 RIS records from the step-1 screened export.

3. Package label meaning: a single pre-full-text label created by joining the title/keyword screen and the abstract screen.

4. Positive label count after import corrections: 142.

## Criteria

1. Criteria source: review paper and screening-decision workbook.

2. Local criteria file: `criteria.R`.

3. The paper states a search restricted to English-language, peer-reviewed journal articles, reviews, and editorials from 2016 to 2022.

4. Step 1 used title and keyword criteria around `sustainability`, `cities`, `planning`, and `nature`, with a `joker` override for clearly relevant cases.

5. Step 2 used abstract criteria around `cities`, `planning`, `nature`, `EU`, and `empirical` study design.

## Import Notes

1. The paper reports 1,557 records at screening step 1 and 143 records after abstract screening before full-text review.

2. The downloadable `B_01_recordsscreened-title-keywords.ris` contains the full 1,557 step-1 records and is therefore the correct base file for the package.

3. The downloadable RIS export still contains 8 records outside the paper's stated 2016 to 2022 year limit. Those records were removed during import.

4. One additional record has a missing publication year tag and was retained because the source metadata is insufficient to prove it violates the stated year restriction.

5. After applying the year restriction, the packaged dataset contains 1,549 records and 142 positives.

6. The RIS export has incomplete language metadata. Missing language tags were retained because they are not sufficient evidence that a record is non-English.

7. Full-text labels were not imported because this package import is intended to represent the pre-full-text screening phase only.

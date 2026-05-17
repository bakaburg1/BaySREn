# ACE Source Notes

- Local object: `ace`
- Local text-bearing raw file: `data-raw/Ace/data.csv`
- Local gold-standard labels file: `data-raw/Ace/epc-ir.clean.tsv`
- Local PubMed metadata cache: `data-raw/Ace/pubmed_medline.nbib`
- Local review PDF: `data-raw/Ace/ace_review.pdf`
- Builder: `data-raw/Ace/ace.R`
- Upstream labels file used by the builder: `https://dmice.ohsu.edu/cohenaa/epc-ir-data/epc-ir.clean.tsv`
- PubMed metadata source: NCBI E-utilities `efetch.fcgi`, using the PMIDs in the OHSU labels file.
- Review PDF source: `https://www.ohsu.edu/sites/default/files/2019-01/ACEI_Final_Report-and-Evidence-Tables_Update-1_Unshaded_JUN_04.pdf`
- Likely primary source: Chou et al., `Drug Class Review on Angiotensin Converting Enzyme Inhibitors`, Final Report, June 2004.
- Source evidence: the OHSU drug-class review data page describes gold-standard files with `Abstract Triage Status` and `Article Triage Status`, and defines code `I` as included at abstract or article level. The package label uses only `Abstract Triage Status = I`.
- Provenance judgment: usable for the abstract-screening benchmark collection.

## Import Notes

- The previous ASReview-derived CSV contained `2235` records and `41` positives, matching the article-level inclusion count rather than the abstract-triage count.
- The OHSU gold-standard TSV contains `2544` ACE records, with `183` `Abstract Triage Status = I` labels and `41` `Article Triage Status = I` labels.
- PubMed E-utilities returned records for all `2544` ACE PMIDs. PubMed did not contain abstract text for `308` records, including `14` abstract-triage positives.
- OpenAlex, Europe PMC, Crossref, and Semantic Scholar probes did not provide sufficiently reliable abstract recovery for the missing-abstract positive records. PMID `10390455` had plausible OpenAlex text, but it is still excluded because the package keeps only PubMed abstract-bearing records.
- The packaged object excludes all PubMed records without abstract text and now contains `2236` records and `169` positives, with `included` derived from `Abstract Triage Status = I`.

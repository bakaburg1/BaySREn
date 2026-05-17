# Review scope

- Topic: barriers to implementing antimicrobial stewardship programmes in long-term care facilities.
- Population/context: adult residents of long-term care facilities, nursing homes, and similar institutions for the elderly.
- Programme scope: full antimicrobial stewardship programmes.

# Paper-reported sources

- Ovid-MEDLINE
- CINAHL
- Embase
- Cochrane Central
- Reference lists of included articles

# Search timing and limits

- Initial search run: July 22-23, 2021
- Updated search through March 14, 2023
- Paper-reported limit: English-language papers

# Core AMS concept block

The AMS review used the following stewardship concept block, with
database-specific syntax:

```text
Antimicrobial Stewardship
OR ((antibiotic* OR anti-biotic* OR antimicrobi* OR anti-microbi*)
NEAR/ADJ 5 stewardship)
OR (microbial drug resistance NEAR/ADJ 3 (prevent* OR control*))
OR ((appropriate OR inappropriate) NEAR/ADJ 4 prescri*)
```

# Core LTCF concept block

```text
Homes for the Aged
OR Residential Facilities
OR Long-Term Care
OR ((long term OR residential OR assisted living OR nursing)
NEAR/ADJ 5 (facilit* OR center* OR centre* OR unit* OR home*))
OR residential facilities restricted to older adults
```

# Example Ovid-MEDLINE combination

```text
1  exp Antimicrobial Stewardship/ or
   ((antibiotic* or anti-biotic* or antimicrobi* or anti-microbi*) adj5 stewardship).mp.
2  exp Drug Resistance, Microbial/pc or
   (microbial drug resistance adj3 (prevent* or control*)).mp.
3  ((appropriate or inappropriate) adj4 prescri*).mp. or Inappropriate Prescribing/
4  1 or 2 or 3
5  Homes for the Aged.mp. or exp Homes for the Aged/
6  Residential Facilities/
7  exp Aged/
8  6 and 7
9  long term care.mp. or exp Long-Term Care/
10 ((long term or residential or assisted living or nursing) adj5
    (facilit* or center* or centre* or unit* or home*)).mp.
11 5 or 8 or 9 or 10
12 4 and 11
```

# Notes

- The full database-specific strings are available in the supplementary
  materials and in the `material/strings/` files.
- This file stores a concise AMS-only transcription for provenance and
  dataset context.

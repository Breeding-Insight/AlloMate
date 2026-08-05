AlloMate — Small Test Dataset
==============================

A minimal, hand-checkable dataset for exercising every input path in the app:
pedigree-based kinship, Matrix Builder (A/G/H via AGHmatrix), and the
"upload a precomputed matrix" option in Mate Allocation.

Population structure
---------------------
18 individuals across 3 generations:
  - 5 founders (unknown parents): M001, M002 (male), F001, F002, F003 (female)
  - 5 generation-1 individuals:    G1_01 .. G1_05
  - 8 candidates (current generation, the mate-allocation pool): C01 .. C08
    (C01/C03/C05/C07 = male, C02/C04/C06/C08 = female)

Files
-----
pedigree.txt            All 18 individuals: id, male_parent, female_parent (0 = unknown).
candidates.txt          The 8 candidates only: id, sex (M/F). Use in Mate Allocation.
length_ebvs.txt         Trait 1 EBVs for the 8 candidates (ID, EBV).
width_ebvs.txt          Trait 2 EBVs for the 8 candidates (ID, EBV).
genotypes.csv           Marker dosages (0/1/2) for 13 genotyped individuals
                        (all 5 gen-1 individuals + all 8 candidates; the 5
                        founders are NOT genotyped, so this also exercises the
                        H matrix's pedigree+genomic blending). 15 SNPs, no
                        missing values.
candidate_A_matrix.csv  A hand-computed pedigree relationship matrix (8x8,
                        candidates only, diagonal = 1+F) — use this to test
                        Mate Allocation's "Upload precomputed matrix (A/G/H)"
                        option directly, without needing AGHmatrix installed.

How to test
-----------
1. Mate Allocation (pedigree route):
   - Candidate list:        candidates.txt
   - Pedigree file:         pedigree.txt
   - Trait EBVs:            length_ebvs.txt, width_ebvs.txt (weight e.g. 0.5/0.5)
   - Run OCS with, e.g., desired inbreeding rate 0.05, 10-20 offspring.

2. Matrix Builder (A):
   - Pedigree file: pedigree.txt -> should report "18 records loaded" with no
     warnings (no unknown-parent surprises beyond the 5 known founders, no
     circular references, no duplicates).

3. Matrix Builder (G):
   - Marker/dosage file: genotypes.csv -> builds a 13x13 genomic matrix.

4. Matrix Builder (H):
   - Pedigree file: pedigree.txt
   - Marker/dosage file: genotypes.csv
   - Builds an 18x18 combined matrix (13 genotyped + 5 pedigree-only founders).
   - Download it, then upload it in Mate Allocation via "Upload precomputed
     matrix (A/G/H)" to run OCS off the H matrix instead of pedigree-only A.
   - Note: G is blended toward the pedigree block A22 (default 0.95*G +
     0.05*A22) before Hmatrix() is called. This is required, not a tuning
     nicety — a VanRaden G is centred by sample allele frequency, which makes
     it singular by construction, and Hmatrix() inverts the genotyped block.
     Without the blend this step fails on every dataset, including this one.
   - "Martini" is the only H method offered, because it is the only one
     AGHmatrix implements that AlloMate can supply arguments for.

5. Mate Allocation (precomputed matrix route, no AGHmatrix needed):
   - Candidate list:        candidates.txt
   - Relationship matrix source: "Upload precomputed matrix (A/G/H)"
   - Matrix file:           candidate_A_matrix.csv
   - Leave "Values are already kinship coefficients" UNCHECKED (this matrix
     is on the relationship scale, diagonal = 1+F; the app halves it
     automatically to the kinship scale).
   - Trait EBVs as in step 1.

Expected sanity checks
----------------------
- candidate_A_matrix.csv diagonal: 1.0000 for C01/C03/C06/C07 (no inbreeding),
  1.1250 for C02/C04/C05/C08 (F = 0.125, since their parents share a
  grandparent on both sides).
- C01 & C07 are full sibs (parents G1_01 x G1_04): relationship = 0.5000.
- C04 & C08 are full sibs (parents G1_02 x G1_05): relationship = 0.6250
  (higher than the usual 0.5 because G1_02 and G1_05 themselves share a
  parent, M001).
- Kinship threshold: a value around 0.06-0.08 will start excluding some of
  the more related candidate pairs, useful for testing the threshold filter.

Note
----
Each generation-1 individual (G1_01..G1_05) is used consistently as either a
male_parent or a female_parent everywhere it appears in pedigree.txt
(G1_01/G1_02/G1_03 = sires, G1_04/G1_05 = dams). If you edit this file, keep
that consistent — clean_pedigree() treats an id used as both a male_parent
and a female_parent as ambiguous and zeroes out both roles for the affected
rows (reported as a warning in the pedigree status box), which silently
turns two-parent relationships into one-parent ones.

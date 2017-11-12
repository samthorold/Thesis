
pandoc -o Literature_Review.pdf --filter pandoc-eqnos --filter pandoc-citeproc ^
    Summaries.md ^
    FamaMacBeth1973_Risk_summary.md ^
    FamaFrench1993_Common_summary.md ^
    FamaFrench2017_International_summary.md ^
    Ref.md
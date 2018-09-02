" Language:     LaTeX
" Maintainer:   Marry Witherspoon
" Last Change:  Wed Feb 13 22:50:42 CST 2013

set spell
au! BufWritePost *.tex call system("pdflatex -interaction batchmode " . expand('<afile>' . "") . " -output-directory " . expand('<afile>:h' . ""))
"au BufWritePost *.tex call system("scp " . expand('<afile>') . ")

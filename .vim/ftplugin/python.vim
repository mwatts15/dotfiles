" Language:     Python
" Maintainer:   Mark Watts <wattsmark2015@gmail.com>
" Last Change:  2015-07-02

setl ep=autopep8_filter
setl keywordprg=pydoc
let g:syntastic_python_checkers = ["frosted", "pep8"]
let g:syntastic_python_pep8_post_args = "--max-line-length=120 --ignore=E261,E265,E266,E402,E121,E123,E126,E226,E24,E704,E731,E741,E128,W504,W503"
let g:syntastic_python_pylint_post_args = "-j 0 --limit-inference-results=10 --disable=all --enable=unused-import"
setl tw=90
syntax sync minlines=50

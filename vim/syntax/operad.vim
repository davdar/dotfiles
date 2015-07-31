" Vim syntax file
" Language: Operad Programming Language (OPL)
" Maintainer: David Darais (david.darais@gmail.com)
" Latest Revision: Feb 4, 2014

if exists("b:current_syntax")
  finish
endif

syntax clear

highlight def link oplKeyword          Keyword
highlight          oplPunctuation      ctermfg=Gray guifg=Gray
highlight def link oplSingleComment    Comment
highlight def link oplMultiComment     Comment
highlight def link oplBinderName       Identifier
highlight def link oplRenamingTarget   Identifier
highlight def link oplSpecial          Special

""""" keywords

set iskeyword+=.
syntax keyword oplKeyword algebra
syntax keyword oplKeyword all
syntax keyword oplKeyword apply
syntax keyword oplKeyword box
syntax keyword oplKeyword define
syntax keyword oplKeyword end
syntax keyword oplKeyword external
syntax keyword oplKeyword import
syntax keyword oplKeyword internal
syntax keyword oplKeyword module
syntax keyword oplKeyword none
syntax keyword oplKeyword only
syntax keyword oplKeyword plug
syntax keyword oplKeyword provide
syntax keyword oplKeyword qualified
syntax keyword oplKeyword require
syntax keyword oplKeyword where
syntax keyword oplKeyword wiring

""""" punctuation

syntax match oplPunctuation "("
syntax match oplPunctuation ")"
syntax match oplPunctuation ","
syntax match oplPunctuation "->"
syntax match oplPunctuation ":"
syntax match oplPunctuation ":="
syntax match oplPunctuation "<-"
syntax match oplPunctuation "=>"
syntax match oplPunctuation "=\[\]="
syntax match oplPunctuation "@"
syntax match oplPunctuation "\["
syntax match oplPunctuation "\]"
syntax match oplPunctuation "_"

""""" comments

syntax match oplSingleComment "#.*$"
syntax region oplMultiComment start="#|" end="|#" contains=oplMultiComment

""""" binders

syntax match oplBinder                     "\(\w\|[_']\)\+\s*:"                  contains=oplBinderName,oplPunctuation
syntax match oplBinderName       contained "\(\w\|[_']\)\+"

""""" regions

"syntax region oplBox       keepend   start="box" end="end" contains=oplKeyword,oplPunctuation,oplBoxCommand
"syntax region oplBoxCommand keepend start="input" end=";"              nextgroup=oplBinderList
"syntax region oplBoxCommand keepend start="output" end=";"             nextgroup=oplBinderList

"syntax region oplWiringDiagram keepend start="wiring diagram" end="end" contains=oplKeyword,oplPunctuation
"syntax match oplWiringDiagramCommand "internal" nextgroup=oplSpecial contained
"syntax match oplWiringDiagramCommand "external\s\+box" nextgroup=oplClassifier contained

"syntax region oplWiringComposition keepend start="wiring composition" end="end" contains=oplKeyword,oplPunctuation
"syntax match oplWiringCompositionCommand "internal w.d." nextgroup=oplBinder contained

"syntax match oplSpecial ".*" contained


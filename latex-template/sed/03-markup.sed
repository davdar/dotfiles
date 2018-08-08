s/⇡→\*/\\overrightarrow /g
s/⇡→/\\vec /g
s/⇡~\*/\\widetilde /g
s/⇡~/\\tilde /g
s/⇡\^\*/\\widehat /g
s/⇡\^/\\hat /g
s/⇡-\*/\\overline /g
s/⇡-/\\bar /g
s/⇡\./\\dot /g
s/⇡‥/\\ddot /g
s/⇡…/\\dddot /g
s/⇡u/\\breve /g
s/⇡v/\\check /g
s/⇡o/\\mathring /g
s/⇡w\*/\\overbrace/g
s/⇡e\*/\\overbracket/g

s/⇣-\*/\\underline /g
s/⇣w\*/\\underbrace/g
s/⇣e\*/\\underbracket/g

s/⇡/\\overset /g
s/⇣/\\underset /g

s/“/``/g
s/”/''/g

s/❬/\\label{/g
s/❭/}/g
s/❪/\\tag{/g
s/❫/}/g
s/⦉/~\\ref{/g
s/⦊/}/g
s/⦇/\\eqref{/g
s/⦈/}/g
s/⟬𝔞/\\citeauthor{/g
s/⟬/\\citet{/g
s/⟭/}/g
s/⦅𝔶/~\\citeyearpar{/g
s/⦅/~\\citep{/g
s/⦆/}/g

s/⸨/{{\\color{\\colorMATH}\\ensuremath{/g
s/⸩/}}}/g
s/‹/{{\\color{\\colorMATH}\\ensuremath{\\operatorname{/g
s/›/}}}}/g

s/⟪/{{\\color{\\colorTEXT}\\text{/g
s/⟫/}}}/g
s/«\*/{{\\color{\\colorTEXT}\\textbf{/g
s/\*»/}}}/g
s/«/{{\\color{\\colorTEXT}\\textit{/g
s/»/}}}/g
s/⦗/{{\\color{\\colorTEXT}\\textsc{\\scriptsize /g
s/⦘/}}}/g

s/⦑/{{\\color{\\colorSYNTAX}\\texttt{/g
s/⦒/}}}/g

s/⋖/\\verb|/g
s/⋗/|/g

s/⌄/\\footnote/g

s/𝑚𝑐(.)(.)/\\multicolumn{\1}{\2}/g

s/Z⁃//g

s/I⁅/\\begin{itemize}[label=\\textbf{-},leftmargin=*]\\item /g
s/I⁃/\\item /g
s/I⁆/\\end{itemize}/g

s/J⁅/\\begin{itemize}[label={},leftmargin=0pt]\\item /g
s/J⁃/\\item /g
s/J⁆/\\end{itemize}/g

s/E⁅/\\begin{enumerate}\\item /g
s/E⁃/\\item /g
s/E⁆/\\end{enumerate}/g

s/Q⁅/\\begin{quote}/g
s/Q⁆/\\end{quote}/g

s/F⁅\[H\]/\\begin{figure}[H]/g
s/F⁅/\\begin{figure}/g
s/⧘/\\caption{/g
s/⧙/}/g
s/F⁆/\\end{figure}/g

s/D⁅/\\begin{framed}/g
s/D⁆/\\end{framed}/g

s/M⁅/\\begingroup\\color{\\colorMATH}\\begin{gather*}/g
s/M⁃/\\cr /g
s/M⁆/\\end{gather*}\\endgroup/g

s/X⁅/\\begin{tabularx}{\\linewidth}{>{\\centering\\arraybackslash\\(}X<{\\)}}/g
s/X⁃/\\cr /g
s/X⁆/\\end{tabularx}/g

s/Aː\[t\]/\\begin{array}[t]{/g
s/Aː/\\begin{array}{/g
s/A⁅/}/g
s/A⁃/\\cr /g
s/A⁆/\\end{array}/g

s/T⁅/\\begin{tabular}/g
s/T⁃/\\cr /g
s/T⁆/\\end{tabular}/g

s/P⁅/\\begingroup\\color{\\colorMATH}\\begin{mathpar}/g
s/P⁃/\\and/g
s/P◘/\\\\/g
s/P⁆/\\end{mathpar}\\endgroup/g

s/Rː⁅/\\inferrule{/g
s/Rː⁃/\\and /g
s/Rː⁆/}/g
s/Rː/\\inferrule[/g
s/R⁅/]{/g
s/R⁃/\\\\/g
s/R◘/\\\\\\\\/g
s/----[-]*/}{/g
s/R⁆/}/g

s/B⁅/\\begingroup\\color{\\colorTEXT}\\boxed{\\begingroup\\color{\\colorMATH}/g
s/B⁆/\\endgroup}\\endgroup/g

s/V⁅/\\begin{verbatim}/g
s/V⁆/\\end{verbatim}/g

s/⁅⁅/\\parbox{\\linewidth}{/g
s/⁆⁆/}/g

s/‘/\\left/g
s/’/\\right/g
s/⫾/␣\\middle|␣/g

s/⧼/\&{}/g
s/⧽/{}\&/g

s/␣/\\hspace*{0.33em}/g
s/꘍/\\hspace*{0.66em}/g
s/␠/\\hspace*{1.00em}/g
s/⩊/\\hfill\\hspace{0pt}/g

s/‗/\\underline{\\hspace{0.66em}}/g

s/⸤/_{/g
s/⸥/}/g
s/⸢/^{/g
s/⸣/}/g

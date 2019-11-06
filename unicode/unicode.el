(quail-define-package
 "darais"
 "UTF-32"
 "DaraisInput"
 t
 "David Darais's personal input codes"
 nil t nil nil nil nil nil nil nil nil t)
(quail-define-rules
 ("\\\\" ["\\"])
 ("\\u" ["↑"])
 ("\\r" ["→"])
 ("\\d" ["↓"])
 ("\\l" ["←"])
 ("\\ur" ["↗"])
 ("\\dr" ["↘"])
 ("\\dl" ["↙"])
 ("\\ul" ["↖"])
 ("\\ud-" ["↕"])
 ("\\rl-" ["↔"])
 ("\\uu" ["⇈"])
 ("\\rr" ["⇉"])
 ("\\dd" ["⇊"])
 ("\\ll" ["⇇"])
 ("\\ud" ["⇅"])
 ("\\du" ["⇵"])
 ("\\rl" ["⇄"])
 ("\\lr" ["⇆"])
 ("\\u=" ["⇑"])
 ("\\r=" ["⇒"])
 ("\\d=" ["⇓"])
 ("\\l=" ["⇐"])
 ("\\ud=" ["⇕"])
 ("\\rl=" ["⇔"])
 ("\\r=/" ["⇏"])
 ("\\l=/" ["⇍"])
 ("\\rl=/" ["⇎"])
 ("\\ur=" ["⇗"])
 ("\\dr=" ["⇘"])
 ("\\dl=" ["⇙"])
 ("\\ul=" ["⇖"])
 ("\\r==" ["⇛"])
 ("\\l==" ["⇚"])
 ("\\impr" ["⟹"])
 ("\\impl" ["⟸"])
 ("\\iff" ["⟺"])
 ("\\u|" ["⇧"])
 ("\\r|" ["⇨"])
 ("\\d|" ["⇩"])
 ("\\l|" ["⇦"])
 ("\\rl|" ["⬄"])
 ("\\r|=" ["⇰"])
 ("\\r\\\\" ["⇀"])
 ("\\u|-" ["↥"])
 ("\\r|-" ["↦"])
 ("\\d|-" ["↧"])
 ("\\l|-" ["↤"])
 ("\\rh" ["↪"])
 ("\\lh" ["↩"])
 ("\\rc" ["↬"])
 ("\\lc" ["↫"])
 ("\\r>|" ["⇥"])
 ("\\l>|" ["⇤"])
 ("\\u>>" ["↟"])
 ("\\r>>" ["↠"])
 ("\\d>>" ["↡"])
 ("\\l>>" ["↞"])
 ("\\r|>" ["⇾"])
 ("\\l|>" ["⇽"])
 ("\\rl|>" ["⇿"])
 ("\\r~" ["↝"])
 ("\\l~" ["↜"])
 ("\\rl~" ["↭"])
 ("\\rv" ["↣"])
 ("\\lv" ["↢"])
 ("\\r~~" ["⇝"])
 ("\\l~~" ["⇜"])
 ("\\u." ["⇡"])
 ("\\r." ["⇢"])
 ("\\d." ["⇣"])
 ("\\l." ["⇠"])
 ("\\r/" ["↛"])
 ("\\l/" ["↚"])
 ("\\rl/" ["↮"])
 ("\\ro" ["⊸"])
 ("\\lo" ["⟜"])
 ("\\rlo" ["⧟"])
 ("\\zd" ["↯"])
 ("\\cw" ["↻"])
 ("\\ccw" ["↺"])
 ("\\(" ["⟮"])
 ("\\)" ["⟯"])
 ("\\((" ["⸨"])
 ("\\))" ["⸩"])
 ("\\(|" ["⦇"])
 ("\\)|" ["⦈"])
 ("\\c(" ["⦅"])
 ("\\c)" ["⦆"])
 ("\\b(" ["❪"])
 ("\\b)" ["❫"])
 ("\\B(" ["❨"])
 ("\\B)" ["❩"])
 ("\\u)" ["⏜"])
 ("\\d)" ["⏝"])
 ("\\fr" ["⌢"])
 ("\\sm" ["⌣"])
 ("\\<" ["⟨"])
 ("\\>" ["⟩"])
 ("\\<<" ["⟪"])
 ("\\>>" ["⟫"])
 ("\\<|" ["⦉"])
 ("\\>|" ["⦊"])
 ("\\<." ["⦑"])
 ("\\>." ["⦒"])
 ("\\b<" ["❬"])
 ("\\b>" ["❭"])
 ("\\<\"" ["«"])
 ("\\>\"" ["»"])
 ("\\<'" ["‹"])
 ("\\>'" ["›"])
 ("\\(<" ["⦓"])
 ("\\)>" ["⦔"])
 ("\\((>" ["⦕"])
 ("\\))<" ["⦖"])
 ("\\u<" ["⸦"])
 ("\\u>" ["⸧"])
 ("\\f<" ["᚜"])
 ("\\f>" ["᚛"])
 ("\\_<" ["⸜"])
 ("\\_>" ["⸝"])
 ("\\^<" ["⸌"])
 ("\\^>" ["⸍"])
 ("\\^L<" ["⸂"])
 ("\\^L>" ["⸃"])
 ("\\^D<" ["⸄"])
 ("\\^D>" ["⸅"])
 ("\\^S<" ["⸉"])
 ("\\^S>" ["⸊"])
 ("\\[" ["⦗"])
 ("\\]" ["⦘"])
 ("\\[[" ["⟦"])
 ("\\]]" ["⟧"])
 ("\\[|" ["⟬"])
 ("\\]|" ["⟭"])
 ("\\e[" ["⁅"])
 ("\\e]" ["⁆"])
 ("\\tL" ["⌈"])
 ("\\tR" ["⌉"])
 ("\\bL" ["⌊"])
 ("\\bR" ["⌋"])
 ("\\tl" ["⸢"])
 ("\\tr" ["⸣"])
 ("\\bl" ["⸤"])
 ("\\br" ["⸥"])
 ("\\{|" ["⦃"])
 ("\\}|" ["⦄"])
 ("\\{" ["⎨"])
 ("\\}" ["⎬"])
 ("\\z{" ["⧘"])
 ("\\z}" ["⧙"])
 ("\\z{{" ["⧚"])
 ("\\z}}" ["⧛"])
 ("\\b{" ["❴"])
 ("\\b}" ["❵"])
 ("\\s{" ["⟅"])
 ("\\s}" ["⟆"])
 ("\\</" ["≮"])
 ("\\<=" ["≤"])
 ("\\>=" ["≥"])
 ("\\<=>=" ["⋚"])
 ("\\<?" ["⩻"])
 ("\\>?" ["⩼"])
 ("\\><" ["⪥"])
 ("\\><x" ["⪤"])
 ("\\<2" ["≪"])
 ("\\>2" ["≫"])
 ("\\<3" ["⋘"])
 ("\\>3" ["⋙"])
 ("\\>>=" ["≫="])
 ("\\<<=" ["≪="])
 ("\\xo|" ["⊻"])
 ("\\nand" ["⊼"])
 ("\\nor" ["⊽"])
 ("\\h<" ["⪡"])
 ("\\h>" ["⪢"])
 ("\\hor" ["⩔"])
 ("\\hand" ["⩓"])
 ("\\Hand" ["⩕"])
 ("\\Hor" ["⩖"])
 ("\\<o" ["⩹"])
 ("\\>o" ["⩺"])
 ("\\d<" ["⋖"])
 ("\\d>" ["⋗"])
 ("\\d<=" ["⩿"])
 ("\\d>=" ["⪀"])
 ("\\dor" ["⟇"])
 ("\\dand" ["⟑"])
 ("\\m<" ["⪪"])
 ("\\m>" ["⪫"])
 ("\\m<=" ["⪬"])
 ("\\m>=" ["⪭"])
 ("\\m<|" ["⩤"])
 ("\\m>|" ["⩥"])
 ("\\mand" ["⩚"])
 ("\\mor" ["⩛"])
 ("\\e<" ["≺"])
 ("\\e>" ["≻"])
 ("\\e<-" ["⪯"])
 ("\\e>-" ["⪰"])
 ("\\e<=" ["≼"])
 ("\\e>=" ["≽"])
 ("\\eor" ["⋎"])
 ("\\eand" ["⋏"])
 ("\\e<<" ["⪻"])
 ("\\e>>" ["⪼"])
 ("\\t<=" ["⊴"])
 ("\\t>=" ["⊵"])
 ("\\t<" ["⊲"])
 ("\\t>" ["⊳"])
 ("\\t<>" ["⧎"])
 ("\\t<|" ["⧏"])
 ("\\t>|" ["⧐"])
 ("\\t<=/" ["⋬"])
 ("\\t>=/" ["⋭"])
 ("\\ct<" ["⪦"])
 ("\\ct>" ["⪧"])
 ("\\ct<=" ["⪨"])
 ("\\ct>=" ["⪩"])
 ("\\ctor" ["⌔"])
 ("\\y<" ["⧼"])
 ("\\y>" ["⧽"])
 ("\\Y<" ["⊰"])
 ("\\Y>" ["⊱"])
 ("\\s<" ["⊂"])
 ("\\s>" ["⊃"])
 ("\\s<=" ["⊆"])
 ("\\s>=" ["⊇"])
 ("\\s<=/" ["⊈"])
 ("\\s>=/" ["⊉"])
 ("\\s</=" ["⊊"])
 ("\\s>/=" ["⊋"])
 ("\\s</" ["⊄"])
 ("\\s>/" ["⊅"])
 ("\\su" ["∪"])
 ("\\si" ["∩"])
 ("\\sU" ["⋃"])
 ("\\sI" ["⋂"])
 ("\\su+" ["⊎"])
 ("\\sU+" ["⨄"])
 ("\\s<." ["⪽"])
 ("\\s>." ["⪾"])
 ("\\su." ["⊍"])
 ("\\si." ["⩀"])
 ("\\s<|" ["⫏"])
 ("\\s>|" ["⫐"])
 ("\\s<=|" ["⫑"])
 ("\\s>=|" ["⫒"])
 ("\\su|" ["⩌"])
 ("\\si|" ["⩍"])
 ("\\hs<" ["⋐"])
 ("\\hs>" ["⋑"])
 ("\\hsu" ["⋓"])
 ("\\hsi" ["⋒"])
 ("\\q<=" ["⊑"])
 ("\\q>=" ["⊒"])
 ("\\q<" ["⊏"])
 ("\\q>" ["⊐"])
 ("\\q<=/" ["⋢"])
 ("\\q>=/" ["⋣"])
 ("\\q</=" ["⋤"])
 ("\\q>/=" ["⋥"])
 ("\\j" ["⊔"])
 ("\\m" ["⊓"])
 ("\\J" ["⨆"])
 ("\\M" ["⨅"])
 ("\\hj" ["⩏"])
 ("\\hm" ["⩎"])
 ("\\w" ["⩊"])
 ("\\sum" ["∑"])
 ("\\prod" ["∏"])
 ("\\'<" ["‘"])
 ("\\'>" ["’"])
 ("\\'`" ["‛"])
 ("\\\"<" ["“"])
 ("\\\">" ["”"])
 ("\\,," ["„"])
 ("\\\"`" ["‟"])
 ("\\--" ["–"])
 ("\\---" ["—"])
 ("\\'" ["′"])
 ("\\''" ["″"])
 ("\\'''" ["‴"])
 ("\\`" ["‵"])
 ("\\``" ["‶"])
 ("\\```" ["‷"])
 ("\\." ["⋅"])
 ("\\.bu" ["∙"])
 ("\\:" ["∶"])
 ("\\:3" ["⁝"])
 ("\\:4" ["⁞"])
 ("\\t:" ["ː"])
 ("\\s:" ["꞉"])
 ("\\ther" ["∴"])
 ("\\bec" ["∵"])
 ("\\::" ["∷"])
 ("\\.." ["‥"])
 ("\\..." ["…"])
 ("\\c..." ["⋯"])
 ("\\v..." ["⋮"])
 ("\\.:" ["⁖"])
 ("\\.:." ["⁘"])
 ("\\:.:" ["⁙"])
 ("\\.+" ["⸭"])
 ("\\o" ["∘"])
 ("\\o:" ["⦂"])
 ("\\o;" ["⨟"])
 ("\\,|" ["⍪"])
 ("\\d^" ["⌄"])
 ("\\.^" ["˙"])
 ("\\and," ["꘍"])
 ("\\|" ["∣"])
 ("\\|/" ["∤"])
 ("\\|:" ["¦"])
 ("\\w|" ["⫾"])
 ("\\||" ["‖"])
 ("\\par" ["∥"])
 ("\\%" ["÷"])
 ("\\%%" ["٪"])
 ("\\/" ["∕"])
 ("\\set\\" ["∖"])
 ("\\//" ["⫽"])
 ("\\&" ["⅋"])
 ("\\///" ["⫻"])
 ("\\O/" ["∅"])
 ("\\*" ["⋆"])
 ("\\star" ["☆"])
 ("\\starb" ["★"])
 ("\\**" ["⋇"])
 ("\\#" ["♯"])
 ("\\##" ["𝄪"])
 ("\\b" ["♭"])
 ("\\bb" ["𝄫"])
 ("\\n" ["♮"])
 ("\\=||" ["⋕"])
 ("\\d?" ["¿"])
 ("\\d!" ["¡"])
 ("\\??" ["⁇"])
 ("\\!!" ["‼"])
 ("\\?!" ["⁈"])
 ("\\!?" ["⁉"])
 ("\\++" ["⧺"])
 ("\\+++" ["⧻"])
 ("\\+." ["∔"])
 ("\\+(" ["⨭"])
 ("\\+)" ["⨮"])
 ("\\+-" ["±"])
 ("\\-." ["∸"])
 ("\\x" ["×"])
 ("\\x." ["⨰"])
 ("\\x-" ["⨱"])
 ("\\x(" ["⨴"])
 ("\\x)" ["⨵"])
 ("\\xx" ["⨯"])
 ("\\XX" ["⨳"])
 ("\\xb" ["✖"])
 ("\\swap" ["⤨"])
 ("\\o+" ["⊕"])
 ("\\o-" ["⊖"])
 ("\\ox" ["⊗"])
 ("\\o/" ["⊘"])
 ("\\o." ["⊙"])
 ("\\oo" ["⊚"])
 ("\\o*" ["⊛"])
 ("\\o=" ["⊜"])
 ("\\o--" ["⊝"])
 ("\\ot" ["⎊"])
 ("\\o|" ["⦶"])
 ("\\o||" ["⦷"])
 ("\\o\\" ["⦸"])
 ("\\obot" ["⦹"])
 ("\\o%" ["⦼"])
 ("\\o<" ["⧀"])
 ("\\o>" ["⧁"])
 ("\\.di" ["⋄"])
 ("\\di" ["◇"])
 ("\\di." ["⟐"])
 ("\\s" ["□"])
 ("\\s+" ["⊞"])
 ("\\s-" ["⊟"])
 ("\\sx" ["⊠"])
 ("\\s." ["⊡"])
 ("\\sand" ["⟎"])
 ("\\sor" ["⟏"])
 ("\\s/" ["⧄"])
 ("\\s\\" ["⧅"])
 ("\\s*" ["⧆"])
 ("\\so" ["⧇"])
 ("\\ss" ["⧈"])
 ("\\sss" ["⧉"])
 ("\\s|" ["⎅"])
 ("\\O" ["○"])
 ("\\qed" ["∎"])
 ("\\-/" ["⌿"])
 ("\\sin" ["∿"])
 ("\\link" ["∾"])
 ("\\bow" ["⋈"])
 ("\\hour" ["⧖"])
 ("\\bowl" ["⋉"])
 ("\\bowr" ["⋊"])
 ("\\inf" ["∞"])
 ("\\inf<" ["∝"])
 ("\\tru" ["△"])
 ("\\trr" ["▷"])
 ("\\trd" ["▽"])
 ("\\trl" ["◁"])
 ("\\trub" ["▲"])
 ("\\trrb" ["▶"])
 ("\\trdb" ["▼"])
 ("\\trlb" ["◀"])
 ("\\t+" ["⨹"])
 ("\\t-" ["⨺"])
 ("\\tx" ["⨻"])
 ("\\tt" ["⟁"])
 ("\\t." ["◬"])
 ("\\cd" ["⟡"])
 ("\\cd>" ["⟢"])
 ("\\cd<" ["⟣"])
 ("\\sq>" ["⟤"])
 ("\\sq<" ["⟥"])
 ("\\loz" ["⌑"])
 ("\\d<>" ["⟠"])
 ("\\zap" ["⌁"])
 ("\\dag" ["†"])
 ("\\ddag" ["‡"])
 ("\\***" ["⁂"])
 ("\\int" ["∫"])
 ("\\cent" ["¢"])
 ("\\Oo" ["⧂"])
 ("\\+br" ["⌌"])
 ("\\+bl" ["⌍"])
 ("\\+tr" ["⌎"])
 ("\\+tl" ["⌏"])
 ("\\ctl" ["⌜"])
 ("\\ctr" ["⌝"])
 ("\\cbl" ["⌞"])
 ("\\cbr" ["⌟"])
 ("\\>-" ["⌲"])
 ("\\scales" ["⚖"])
 ("\\root" ["√"])
 ("\\in" ["∈"])
 ("\\in." ["⋵"])
 ("\\in|" ["⋳"])
 ("\\in=" ["⋹"])
 ("\\in-" ["⋲"])
 ("\\in-l" ["⋺"])
 ("\\in/" ["∉"])
 ("\\inl" ["∋"])
 ("\\in|l" ["⋻"])
 ("\\inu" ["⟒"])
 ("\\ind" ["⫙"])
 ("\\ele" ["∊"])
 ("\\elel" ["∍"])
 ("\\ele|" ["⋴"])
 ("\\ele|l" ["⋼"])
 ("\\mem" ["⋿"])
 ("\\all" ["∀"])
 ("\\ex" ["∃"])
 ("\\ex/" ["∄"])
 ("\\|-" ["⊢"])
 ("\\-|" ["⊣"])
 ("\\|=" ["⊨"])
 ("\\or" ["∨"])
 ("\\and" ["∧"])
 ("\\andor" ["⩙"])
 ("\\bot" ["⊥"])
 ("\\bot=" ["⫫"])
 ("\\top" ["⊤"])
 ("\\not" ["¬"])
 ("\\xor" ["⊻"])
 ("\\comp" ["∁"])
 ("\\==" ["≡"])
 ("\\==/" ["≢"])
 ("\\===" ["≣"])
 ("\\=2" ["⩵"])
 ("\\=3" ["⩶"])
 ("\\~=" ["≃"])
 ("\\~~" ["≈"])
 ("\\~~/" ["≉"])
 ("\\~" ["∼"])
 ("\\~/" ["≁"])
 ("\\:=" ["≔"])
 ("\\=:" ["≕"])
 ("\\::=" ["⩴"])
 ("\\=/" ["≠"])
 ("\\=?" ["≟"])
 ("\\=o" ["≗"])
 ("\\=*" ["⩮"])
 ("\\=." ["⩦"])
 ("\\=//" ["⧣"])
 ("\\=s" ["≌"])
 ("\\=star" ["≛"])
 ("\\~~^" ["⩯"])
 ("\\=t" ["≜"])
 ("\\=def" ["≝"])
 ("\\eqv" ["≍"])
 ("\\eqv/" ["≭"])
 ("\\=O" ["≎"])
 ("\\block" ["█"])
 ("\\_+" ["₊"])
 ("\\_-" ["₋"])
 ("\\_=" ["₌"])
 ("\\_(" ["₍"])
 ("\\_)" ["₎"])
 ("\\^+" ["⁺"])
 ("\\^-" ["⁻"])
 ("\\^=" ["⁼"])
 ("\\^(" ["⁽"])
 ("\\^)" ["⁾"])
 ("\\^deg" ["˚"])
 ("\\^bow" ["⑅"])
 ("\\^uu" ["ᐜ"])
 ("\\^tr" ["ᐞ"])
 ("\\^/" ["ᐟ"])
 ("\\^\\" ["ᐠ"])
 ("\\^un" ["ᐡ"])
 ("\\^in" ["ᐢ"])
 ("\\^su" ["ᐣ"])
 ("\\^//" ["ᐥ"])
 ("\\^||" ["ᐦ"])
 ("\\^top" ["ᐪ"])
 ("\\^." ["ᐧ"])
 ("\\^^nu" ["֮"])
 ("\\^^^" ["̂"])
 ("\\^^<" ["᷾"])
 ("\\^^>" ["͐"])
 ("\\^^<-" ["⃖"])
 ("\\^^->" ["⃗"])
 ("\\^^~" ["̃"])
 ("\\^^(" ["̑"])
 ("\\^^)" ["̆"])
 ("\\^^~~" ["͌"])
 ("\\^^-" ["̄"])
 ("\\^^--" ["̅"])
 ("\\^^." ["̇"])
 ("\\^^.." ["̈"])
 ("\\^^o" ["̊"])
 ("\\^^," ["̉"])
 ("\\^^v" ["̌"])
 ("\\^^(." ["̐"])
 ("\\PP" ["¶"])
 ("\\_" ["␣"])
 ("\\__" ["‗"])
 ("\\___" ["﹍"])
 ("\\~_" ["﹏"])
 ("\\utie" ["‿"])
 ("\\iutie" ["⁔"])
 ("\\sp" ["␠"])
 ("\\SS" ["§"])
 ("\\bu" ["•"])
 ("\\obu" ["◦"])
 ("\\-bu" ["⁃"])
 ("\\tbu" ["‣"])
 ("\\lbu" ["⁌"])
 ("\\rbu" ["⁍"])
 ("\\ibu" ["◘"])
 ("\\hbu" ["❥"])
 ("\\hlbu" ["☙"])
 ("\\hrbu" ["❧"])
 ("\\buo" ["⦿"])
 ("\\obuo" ["⦾"])
 ("\\check" ["✓"])
 ("\\X" ["✗"])
 ("\\bal" ["☐"])
 ("\\balc" ["☑"])
 ("\\balx" ["☒"])
 ("\\disapprove" ["ಠ_ಠ"])
 ("\\flip" ["﹙╯°□°）╯︵┻━┻"])
 ("\\putback" ["┬─┬ノ﹙゜-゜ノ﹚"])
 ("\\dust" ["┬─┬⃰͡ ﹙ᵔᵕᵔ͜ ﹚"])
 ("\\doubleflip" ["┻━┻︵ヽ﹙`Д´﹚ﾉ︵┻━┻"])
 ("\\fisticuffs" ["ლ﹙｀ー´ლ﹚"])
 ("\\shrug" ["¯\\_﹙ツ﹚_/¯"])
 ("\\meh" ["¯\\﹙°_o﹚/¯"])
 ("\\angry" ["﹙╬ಠ益ಠ﹚"])
 ("\\meow" ["ฅ^•ﻌ•^ฅ"])
 ("\\cheers" ["﹙^_^）o自自o（^_^﹚"])
 ("\\crying" ["ಥ_ಥ"])
 ("\\breakdown" ["ಥ﹏ಥ"])
 ("\\disagree" ["٩◔̯◔۶"])
 ("\\flex" ["ᕙ﹙⇀‸↼‶﹚ᕗ"])
 ("\\sadconfused" ["¯\\_﹙⊙︿⊙﹚_/¯"])
 ("\\ytho" ["щ（ﾟДﾟщ）"])
 ("\\strut" ["ᕕ﹙ᐛ﹚ᕗ"])
 ("\\fkit" ["t﹙-_-t﹚"])
 ("\\sleepy" ["눈_눈"])
 ("\\opera" ["ヾ﹙´〇`﹚ﾉ♪♪♪"])
 ("\\zombie" ["[¬º-°]¬"])
 ("\\point" ["﹙☞ﾟヮﾟ﹚☞"])
 ("\\poo" ["💩"])
 ("\\Alpha" ["Α"])
 ("\\Beta" ["Β"])
 ("\\Gamma" ["Γ"])
 ("\\Delta" ["Δ"])
 ("\\Epsilon" ["Ε"])
 ("\\Zeta" ["Ζ"])
 ("\\Eta" ["Η"])
 ("\\Theta" ["Θ"])
 ("\\Iota" ["Ι"])
 ("\\Kappa" ["Κ"])
 ("\\Lambda" ["Λ"])
 ("\\Mu" ["Μ"])
 ("\\Nu" ["Ν"])
 ("\\Xi" ["Ξ"])
 ("\\Omicron" ["Ο"])
 ("\\Pi" ["Π"])
 ("\\Rho" ["Ρ"])
 ("\\varSigma" ["ϴ"])
 ("\\Sigma" ["Σ"])
 ("\\Tau" ["Τ"])
 ("\\Upsilon" ["Υ"])
 ("\\Phi" ["Φ"])
 ("\\Chi" ["Χ"])
 ("\\Psi" ["Ψ"])
 ("\\Omega" ["Ω"])
 ("\\Nabla" ["∇"])
 ("\\alpha" ["α"])
 ("\\beta" ["β"])
 ("\\gamma" ["γ"])
 ("\\delta" ["δ"])
 ("\\epsilon" ["ε"])
 ("\\zeta" ["ζ"])
 ("\\eta" ["η"])
 ("\\theta" ["θ"])
 ("\\iota" ["ι"])
 ("\\kappa" ["κ"])
 ("\\lambda" ["λ"])
 ("\\mu" ["μ"])
 ("\\nu" ["ν"])
 ("\\xi" ["ξ"])
 ("\\omicron" ["ο"])
 ("\\pi" ["π"])
 ("\\rho" ["ρ"])
 ("\\varsigma" ["ς"])
 ("\\sigma" ["σ"])
 ("\\tau" ["τ"])
 ("\\upsilon" ["υ"])
 ("\\phi" ["φ"])
 ("\\chi" ["χ"])
 ("\\psi" ["ψ"])
 ("\\omega" ["ω"])
 ("\\nabla" ["∂"])
 ("\\varepsilon" ["ϵ"])
 ("\\vartheta" ["ϑ"])
 ("\\varkappa" ["ϰ"])
 ("\\varphi" ["ϕ"])
 ("\\varrho" ["ϱ"])
 ("\\varpi" ["ϖ"])
 ("\\lambda/" ["ƛ"])
 ("\\bdAlpha" ["𝚨"])
 ("\\bdBeta" ["𝚩"])
 ("\\bdGamma" ["𝚪"])
 ("\\bdDelta" ["𝚫"])
 ("\\bdEpsilon" ["𝚬"])
 ("\\bdZeta" ["𝚭"])
 ("\\bdEta" ["𝚮"])
 ("\\bdTheta" ["𝚯"])
 ("\\bdIota" ["𝚰"])
 ("\\bdKappa" ["𝚱"])
 ("\\bdLambda" ["𝚲"])
 ("\\bdMu" ["𝚳"])
 ("\\bdNu" ["𝚴"])
 ("\\bdXi" ["𝚵"])
 ("\\bdOmicron" ["𝚶"])
 ("\\bdPi" ["𝚷"])
 ("\\bdRho" ["𝚸"])
 ("\\bdvarSigma" ["𝚹"])
 ("\\bdSigma" ["𝚺"])
 ("\\bdTau" ["𝚻"])
 ("\\bdUpsilon" ["𝚼"])
 ("\\bdPhi" ["𝚽"])
 ("\\bdChi" ["𝚾"])
 ("\\bdPsi" ["𝚿"])
 ("\\bdOmega" ["𝛀"])
 ("\\bdNabla" ["𝛁"])
 ("\\bdalpha" ["𝛂"])
 ("\\bdbeta" ["𝛃"])
 ("\\bdgamma" ["𝛄"])
 ("\\bddelta" ["𝛅"])
 ("\\bdepsilon" ["𝛆"])
 ("\\bdzeta" ["𝛇"])
 ("\\bdldeta" ["𝛈"])
 ("\\bdtheta" ["𝛉"])
 ("\\bdiota" ["𝛊"])
 ("\\bdkappa" ["𝛋"])
 ("\\bdlambda" ["𝛌"])
 ("\\bdmu" ["𝛍"])
 ("\\bdnu" ["𝛎"])
 ("\\bdxi" ["𝛏"])
 ("\\bdomicron" ["𝛐"])
 ("\\bdpi" ["𝛑"])
 ("\\bdrho" ["𝛒"])
 ("\\bdvarsigma" ["𝛓"])
 ("\\bdsigma" ["𝛔"])
 ("\\bdtau" ["𝛕"])
 ("\\bdupsilon" ["𝛖"])
 ("\\bdphi" ["𝛗"])
 ("\\bdchi" ["𝛘"])
 ("\\bdpsi" ["𝛙"])
 ("\\bdomega" ["𝛚"])
 ("\\bdnabla" ["𝛛"])
 ("\\bdvarepsilon" ["𝛜"])
 ("\\bdvartheta" ["𝛝"])
 ("\\bdvarkappa" ["𝛞"])
 ("\\bdvarphi" ["𝛟"])
 ("\\bdvarrho" ["𝛠"])
 ("\\bdvarpi" ["𝛡"])
 ("\\itAlpha" ["𝛢"])
 ("\\itBeta" ["𝛣"])
 ("\\itGamma" ["𝛤"])
 ("\\itDelta" ["𝛥"])
 ("\\itEpsilon" ["𝛦"])
 ("\\itZeta" ["𝛧"])
 ("\\itEta" ["𝛨"])
 ("\\itTheta" ["𝛩"])
 ("\\itIota" ["𝛪"])
 ("\\itKappa" ["𝛫"])
 ("\\itLambda" ["𝛬"])
 ("\\itMu" ["𝛭"])
 ("\\itNu" ["𝛮"])
 ("\\itXi" ["𝛯"])
 ("\\itOmicron" ["𝛰"])
 ("\\itPi" ["𝛱"])
 ("\\itRho" ["𝛲"])
 ("\\itvarSigma" ["𝛳"])
 ("\\itSigma" ["𝛴"])
 ("\\itTau" ["𝛵"])
 ("\\itUpsilon" ["𝛶"])
 ("\\itPhi" ["𝛷"])
 ("\\itChi" ["𝛸"])
 ("\\itPsi" ["𝛹"])
 ("\\itOmega" ["𝛺"])
 ("\\itNabla" ["𝛻"])
 ("\\italpha" ["𝛼"])
 ("\\itbeta" ["𝛽"])
 ("\\itgamma" ["𝛾"])
 ("\\itdelta" ["𝛿"])
 ("\\itepsilon" ["𝜀"])
 ("\\itzeta" ["𝜁"])
 ("\\iteta" ["𝜂"])
 ("\\ittheta" ["𝜃"])
 ("\\itiota" ["𝜄"])
 ("\\itkappa" ["𝜅"])
 ("\\itlambda" ["𝜆"])
 ("\\itmu" ["𝜇"])
 ("\\itnu" ["𝜈"])
 ("\\itxi" ["𝜉"])
 ("\\itomicron" ["𝜊"])
 ("\\itpi" ["𝜋"])
 ("\\itrho" ["𝜌"])
 ("\\itvarsigma" ["𝜍"])
 ("\\itsigma" ["𝜎"])
 ("\\ittau" ["𝜏"])
 ("\\itupsilon" ["𝜐"])
 ("\\itphi" ["𝜑"])
 ("\\itchi" ["𝜒"])
 ("\\itpsi" ["𝜓"])
 ("\\itomega" ["𝜔"])
 ("\\itnabla" ["𝜕"])
 ("\\itvarepsilon" ["𝜖"])
 ("\\itvartheta" ["𝜗"])
 ("\\itvarkappa" ["𝜘"])
 ("\\itvarphi" ["𝜙"])
 ("\\itvarrho" ["𝜚"])
 ("\\itvarpi" ["𝜛"])
 ("\\bditAlpha" ["𝜜"])
 ("\\bditBeta" ["𝜝"])
 ("\\bditGamma" ["𝜞"])
 ("\\bditDelta" ["𝜟"])
 ("\\bditEpsilon" ["𝜠"])
 ("\\bditZeta" ["𝜡"])
 ("\\bditEta" ["𝜢"])
 ("\\bditTheta" ["𝜣"])
 ("\\bditIota" ["𝜤"])
 ("\\bditKappa" ["𝜥"])
 ("\\bditLambda" ["𝜦"])
 ("\\bditMu" ["𝜧"])
 ("\\bditNu" ["𝜨"])
 ("\\bditXi" ["𝜩"])
 ("\\bditOmicron" ["𝜪"])
 ("\\bditPi" ["𝜫"])
 ("\\bditRho" ["𝜬"])
 ("\\bditvarSigma" ["𝜭"])
 ("\\bditSigma" ["𝜮"])
 ("\\bditTau" ["𝜯"])
 ("\\bditUpsilon" ["𝜰"])
 ("\\bditPhi" ["𝜱"])
 ("\\bditChi" ["𝜲"])
 ("\\bditPsi" ["𝜳"])
 ("\\bditOmega" ["𝜴"])
 ("\\bditNabla" ["𝜵"])
 ("\\bditalpha" ["𝜶"])
 ("\\bditbeta" ["𝜷"])
 ("\\bditgamma" ["𝜸"])
 ("\\bditdelta" ["𝜹"])
 ("\\bditepsilon" ["𝜺"])
 ("\\bditzeta" ["𝜻"])
 ("\\bditeta" ["𝜼"])
 ("\\bdittheta" ["𝜽"])
 ("\\bditiota" ["𝜾"])
 ("\\bditkappa" ["𝜿"])
 ("\\bditlambda" ["𝝀"])
 ("\\bditmu" ["𝝁"])
 ("\\bditnu" ["𝝂"])
 ("\\bditxi" ["𝝃"])
 ("\\bditomicron" ["𝝄"])
 ("\\bditpi" ["𝝅"])
 ("\\bditrho" ["𝝆"])
 ("\\bditvarsigma" ["𝝇"])
 ("\\bditsigma" ["𝝈"])
 ("\\bdittau" ["𝝉"])
 ("\\bditupsilon" ["𝝊"])
 ("\\bditphi" ["𝝋"])
 ("\\bditchi" ["𝝌"])
 ("\\bditpsi" ["𝝍"])
 ("\\bditomega" ["𝝎"])
 ("\\bditnabla" ["𝝏"])
 ("\\bditvarepsilon" ["𝝐"])
 ("\\bditvartheta" ["𝝑"])
 ("\\bditvarkappa" ["𝝒"])
 ("\\bditvarphi" ["𝝓"])
 ("\\bditvarrho" ["𝝔"])
 ("\\bditvarpi" ["𝝕"])
 ("\\_beta" ["ᵦ"])
 ("\\_gamma" ["ᵧ"])
 ("\\_rho" ["ᵨ"])
 ("\\_phi" ["ᵩ"])
 ("\\_chi" ["ᵪ"])
 ("\\^alpha" ["ᵅ"])
 ("\\^beta" ["ᵝ"])
 ("\\^gamma" ["ᵞ"])
 ("\\^delta" ["ᵟ"])
 ("\\^epsilon" ["ᵋ"])
 ("\\^theta" ["ᶿ"])
 ("\\^iota" ["ᶥ"])
 ("\\^Phi" ["ᶲ"])
 ("\\^phi" ["ᵠ"])
 ("\\^chi" ["ᵡ"])
 ("\\bbGamma" ["ℾ"])
 ("\\bbgamma" ["ℽ"])
 ("\\bbPi" ["ℿ"])
 ("\\bbpi" ["ℼ"])
 ("\\bbSigma" ["⅀"])
 ("\\bd0" ["𝟎"])
 ("\\bd1" ["𝟏"])
 ("\\bd2" ["𝟐"])
 ("\\bd3" ["𝟑"])
 ("\\bd4" ["𝟒"])
 ("\\bd5" ["𝟓"])
 ("\\bd6" ["𝟔"])
 ("\\bd7" ["𝟕"])
 ("\\bd8" ["𝟖"])
 ("\\bd9" ["𝟗"])
 ("\\bb0" ["𝟘"])
 ("\\bb1" ["𝟙"])
 ("\\bb2" ["𝟚"])
 ("\\bb3" ["𝟛"])
 ("\\bb4" ["𝟜"])
 ("\\bb5" ["𝟝"])
 ("\\bb6" ["𝟞"])
 ("\\bb7" ["𝟟"])
 ("\\bb8" ["𝟠"])
 ("\\bb9" ["𝟡"])
 ("\\_0" ["₀"])
 ("\\_1" ["₁"])
 ("\\_2" ["₂"])
 ("\\_3" ["₃"])
 ("\\_4" ["₄"])
 ("\\_5" ["₅"])
 ("\\_6" ["₆"])
 ("\\_7" ["₇"])
 ("\\_8" ["₈"])
 ("\\_9" ["₉"])
 ("\\^0" ["⁰"])
 ("\\^1" ["¹"])
 ("\\^2" ["²"])
 ("\\^3" ["³"])
 ("\\^4" ["⁴"])
 ("\\^5" ["⁵"])
 ("\\^6" ["⁶"])
 ("\\^7" ["⁷"])
 ("\\^8" ["⁸"])
 ("\\^9" ["⁹"])
 ("\\1/2" ["½"])
 ("\\0/3" ["↉"])
 ("\\1/3" ["⅓"])
 ("\\2/3" ["⅔"])
 ("\\1/4" ["¼"])
 ("\\3/4" ["¾"])
 ("\\1/5" ["⅕"])
 ("\\2/5" ["⅖"])
 ("\\3/5" ["⅗"])
 ("\\4/5" ["⅘"])
 ("\\1/6" ["⅙"])
 ("\\5/6" ["⅚"])
 ("\\1/7" ["⅐"])
 ("\\1/8" ["⅛"])
 ("\\3/8" ["⅜"])
 ("\\5/8" ["⅝"])
 ("\\7/8" ["⅞"])
 ("\\1/9" ["⅑"])
 ("\\1/10" ["⅒"])
 ("\\wp" ["℘"])
 ("\\ell" ["ℓ"])
 ("\\ln" ["㏑"])
 ("\\log" ["㏒"])
 ("\\bdA" ["𝐀"])
 ("\\bdB" ["𝐁"])
 ("\\bdC" ["𝐂"])
 ("\\bdD" ["𝐃"])
 ("\\bdE" ["𝐄"])
 ("\\bdF" ["𝐅"])
 ("\\bdG" ["𝐆"])
 ("\\bdH" ["𝐇"])
 ("\\bdI" ["𝐈"])
 ("\\bdJ" ["𝐉"])
 ("\\bdK" ["𝐊"])
 ("\\bdL" ["𝐋"])
 ("\\bdM" ["𝐌"])
 ("\\bdN" ["𝐍"])
 ("\\bdO" ["𝐎"])
 ("\\bdP" ["𝐏"])
 ("\\bdQ" ["𝐐"])
 ("\\bdR" ["𝐑"])
 ("\\bdS" ["𝐒"])
 ("\\bdT" ["𝐓"])
 ("\\bdU" ["𝐔"])
 ("\\bdV" ["𝐕"])
 ("\\bdW" ["𝐖"])
 ("\\bdX" ["𝐗"])
 ("\\bdY" ["𝐘"])
 ("\\bdZ" ["𝐙"])
 ("\\bda" ["𝐚"])
 ("\\bdb" ["𝐛"])
 ("\\bdc" ["𝐜"])
 ("\\bdd" ["𝐝"])
 ("\\bde" ["𝐞"])
 ("\\bdf" ["𝐟"])
 ("\\bdg" ["𝐠"])
 ("\\bdh" ["𝐡"])
 ("\\bdi" ["𝐢"])
 ("\\bdj" ["𝐣"])
 ("\\bdk" ["𝐤"])
 ("\\bdl" ["𝐥"])
 ("\\bdm" ["𝐦"])
 ("\\bdn" ["𝐧"])
 ("\\bdo" ["𝐨"])
 ("\\bdp" ["𝐩"])
 ("\\bdq" ["𝐪"])
 ("\\bdr" ["𝐫"])
 ("\\bds" ["𝐬"])
 ("\\bdt" ["𝐭"])
 ("\\bdu" ["𝐮"])
 ("\\bdv" ["𝐯"])
 ("\\bdw" ["𝐰"])
 ("\\bdx" ["𝐱"])
 ("\\bdy" ["𝐲"])
 ("\\bdz" ["𝐳"])
 ("\\itA" ["𝐴"])
 ("\\itB" ["𝐵"])
 ("\\itC" ["𝐶"])
 ("\\itD" ["𝐷"])
 ("\\itE" ["𝐸"])
 ("\\itF" ["𝐹"])
 ("\\itG" ["𝐺"])
 ("\\itH" ["𝐻"])
 ("\\itI" ["𝐼"])
 ("\\itJ" ["𝐽"])
 ("\\itK" ["𝐾"])
 ("\\itL" ["𝐿"])
 ("\\itM" ["𝑀"])
 ("\\itN" ["𝑁"])
 ("\\itO" ["𝑂"])
 ("\\itP" ["𝑃"])
 ("\\itQ" ["𝑄"])
 ("\\itR" ["𝑅"])
 ("\\itS" ["𝑆"])
 ("\\itT" ["𝑇"])
 ("\\itU" ["𝑈"])
 ("\\itV" ["𝑉"])
 ("\\itW" ["𝑊"])
 ("\\itX" ["𝑋"])
 ("\\itY" ["𝑌"])
 ("\\itZ" ["𝑍"])
 ("\\ita" ["𝑎"])
 ("\\itb" ["𝑏"])
 ("\\itc" ["𝑐"])
 ("\\itd" ["𝑑"])
 ("\\ite" ["𝑒"])
 ("\\itf" ["𝑓"])
 ("\\itg" ["𝑔"])
 ("\\ith" ["ℎ"])
 ("\\iti" ["𝑖"])
 ("\\itj" ["𝑗"])
 ("\\itk" ["𝑘"])
 ("\\itl" ["𝑙"])
 ("\\itm" ["𝑚"])
 ("\\itn" ["𝑛"])
 ("\\ito" ["𝑜"])
 ("\\itp" ["𝑝"])
 ("\\itq" ["𝑞"])
 ("\\itr" ["𝑟"])
 ("\\its" ["𝑠"])
 ("\\itt" ["𝑡"])
 ("\\itu" ["𝑢"])
 ("\\itv" ["𝑣"])
 ("\\itw" ["𝑤"])
 ("\\itx" ["𝑥"])
 ("\\ity" ["𝑦"])
 ("\\itz" ["𝑧"])
 ("\\bditA" ["𝑨"])
 ("\\bditB" ["𝑩"])
 ("\\bditC" ["𝑪"])
 ("\\bditD" ["𝑫"])
 ("\\bditE" ["𝑬"])
 ("\\bditF" ["𝑭"])
 ("\\bditG" ["𝑮"])
 ("\\bditH" ["𝑯"])
 ("\\bditI" ["𝑰"])
 ("\\bditJ" ["𝑱"])
 ("\\bditK" ["𝑲"])
 ("\\bditL" ["𝑳"])
 ("\\bditM" ["𝑴"])
 ("\\bditN" ["𝑵"])
 ("\\bditO" ["𝑶"])
 ("\\bditP" ["𝑷"])
 ("\\bditQ" ["𝑸"])
 ("\\bditR" ["𝑹"])
 ("\\bditS" ["𝑺"])
 ("\\bditT" ["𝑻"])
 ("\\bditU" ["𝑼"])
 ("\\bditV" ["𝑽"])
 ("\\bditW" ["𝑾"])
 ("\\bditX" ["𝑿"])
 ("\\bditY" ["𝒀"])
 ("\\bditZ" ["𝒁"])
 ("\\bdita" ["𝒂"])
 ("\\bditb" ["𝒃"])
 ("\\bditc" ["𝒄"])
 ("\\bditd" ["𝒅"])
 ("\\bdite" ["𝒆"])
 ("\\bditf" ["𝒇"])
 ("\\bditg" ["𝒈"])
 ("\\bdith" ["𝒉"])
 ("\\bditi" ["𝒊"])
 ("\\bditj" ["𝒋"])
 ("\\bditk" ["𝒌"])
 ("\\bditl" ["𝒍"])
 ("\\bditm" ["𝒎"])
 ("\\bditn" ["𝒏"])
 ("\\bdito" ["𝒐"])
 ("\\bditp" ["𝒑"])
 ("\\bditq" ["𝒒"])
 ("\\bditr" ["𝒓"])
 ("\\bdits" ["𝒔"])
 ("\\bditt" ["𝒕"])
 ("\\bditu" ["𝒖"])
 ("\\bditv" ["𝒗"])
 ("\\bditw" ["𝒘"])
 ("\\bditx" ["𝒙"])
 ("\\bdity" ["𝒚"])
 ("\\bditz" ["𝒛"])
 ("\\calA" ["𝒜"])
 ("\\calB" ["ℬ"])
 ("\\calC" ["𝒞"])
 ("\\calD" ["𝒟"])
 ("\\calE" ["ℰ"])
 ("\\calF" ["ℱ"])
 ("\\calG" ["𝒢"])
 ("\\calH" ["ℋ"])
 ("\\calI" ["ℐ"])
 ("\\calJ" ["𝒥"])
 ("\\calK" ["𝒦"])
 ("\\calL" ["ℒ"])
 ("\\calM" ["ℳ"])
 ("\\calN" ["𝒩"])
 ("\\calO" ["𝒪"])
 ("\\calP" ["𝒫"])
 ("\\calQ" ["𝒬"])
 ("\\calR" ["ℛ"])
 ("\\calS" ["𝒮"])
 ("\\calT" ["𝒯"])
 ("\\calU" ["𝒰"])
 ("\\calV" ["𝒱"])
 ("\\calW" ["𝒲"])
 ("\\calX" ["𝒳"])
 ("\\calY" ["𝒴"])
 ("\\calZ" ["𝒵"])
 ("\\cala" ["𝒶"])
 ("\\calb" ["𝒷"])
 ("\\calc" ["𝒸"])
 ("\\cald" ["𝒹"])
 ("\\cale" ["ℯ"])
 ("\\calf" ["𝒻"])
 ("\\calg" ["ℊ"])
 ("\\calh" ["𝒽"])
 ("\\cali" ["𝒾"])
 ("\\calj" ["𝒿"])
 ("\\calk" ["𝓀"])
 ("\\call" ["𝓁"])
 ("\\calm" ["𝓂"])
 ("\\caln" ["𝓃"])
 ("\\calo" ["ℴ"])
 ("\\calp" ["𝓅"])
 ("\\calq" ["𝓆"])
 ("\\calr" ["𝓇"])
 ("\\cals" ["𝓈"])
 ("\\calt" ["𝓉"])
 ("\\calu" ["𝓊"])
 ("\\calv" ["𝓋"])
 ("\\calw" ["𝓌"])
 ("\\calx" ["𝓍"])
 ("\\caly" ["𝓎"])
 ("\\calz" ["𝓏"])
 ("\\bdcalA" ["𝓐"])
 ("\\bdcalB" ["𝓑"])
 ("\\bdcalC" ["𝓒"])
 ("\\bdcalD" ["𝓓"])
 ("\\bdcalE" ["𝓔"])
 ("\\bdcalF" ["𝓕"])
 ("\\bdcalG" ["𝓖"])
 ("\\bdcalH" ["𝓗"])
 ("\\bdcalI" ["𝓘"])
 ("\\bdcalJ" ["𝓙"])
 ("\\bdcalK" ["𝓚"])
 ("\\bdcalL" ["𝓛"])
 ("\\bdcalM" ["𝓜"])
 ("\\bdcalN" ["𝓝"])
 ("\\bdcalO" ["𝓞"])
 ("\\bdcalP" ["𝓟"])
 ("\\bdcalQ" ["𝓠"])
 ("\\bdcalR" ["𝓡"])
 ("\\bdcalS" ["𝓢"])
 ("\\bdcalT" ["𝓣"])
 ("\\bdcalU" ["𝓤"])
 ("\\bdcalV" ["𝓥"])
 ("\\bdcalW" ["𝓦"])
 ("\\bdcalX" ["𝓧"])
 ("\\bdcalY" ["𝓨"])
 ("\\bdcalZ" ["𝓩"])
 ("\\bdcala" ["𝓪"])
 ("\\bdcalb" ["𝓫"])
 ("\\bdcalc" ["𝓬"])
 ("\\bdcald" ["𝓭"])
 ("\\bdcale" ["𝓮"])
 ("\\bdcalf" ["𝓯"])
 ("\\bdcalg" ["𝓰"])
 ("\\bdcalh" ["𝓱"])
 ("\\bdcali" ["𝓲"])
 ("\\bdcalj" ["𝓳"])
 ("\\bdcalk" ["𝓴"])
 ("\\bdcall" ["𝓵"])
 ("\\bdcalm" ["𝓶"])
 ("\\bdcaln" ["𝓷"])
 ("\\bdcalo" ["𝓸"])
 ("\\bdcalp" ["𝓹"])
 ("\\bdcalq" ["𝓺"])
 ("\\bdcalr" ["𝓻"])
 ("\\bdcals" ["𝓼"])
 ("\\bdcalt" ["𝓽"])
 ("\\bdcalu" ["𝓾"])
 ("\\bdcalv" ["𝓿"])
 ("\\bdcalw" ["𝔀"])
 ("\\bdcalx" ["𝔁"])
 ("\\bdcaly" ["𝔂"])
 ("\\bdcalz" ["𝔃"])
 ("\\frakA" ["𝔄"])
 ("\\frakB" ["𝔅"])
 ("\\frakC" ["ℭ"])
 ("\\frakD" ["𝔇"])
 ("\\frakE" ["𝔈"])
 ("\\frakF" ["𝔉"])
 ("\\frakG" ["𝔊"])
 ("\\frakH" ["ℌ"])
 ("\\frakI" ["ℑ"])
 ("\\frakJ" ["𝔍"])
 ("\\frakK" ["𝔎"])
 ("\\frakL" ["𝔏"])
 ("\\frakM" ["𝔐"])
 ("\\frakN" ["𝔑"])
 ("\\frakO" ["𝔒"])
 ("\\frakP" ["𝔓"])
 ("\\frakQ" ["𝔔"])
 ("\\frakR" ["ℜ"])
 ("\\frakS" ["𝔖"])
 ("\\frakT" ["𝔗"])
 ("\\frakU" ["𝔘"])
 ("\\frakV" ["𝔙"])
 ("\\frakW" ["𝔚"])
 ("\\frakX" ["𝔛"])
 ("\\frakY" ["𝔜"])
 ("\\frakZ" ["ℨ"])
 ("\\fraka" ["𝔞"])
 ("\\frakb" ["𝔟"])
 ("\\frakc" ["𝔠"])
 ("\\frakd" ["𝔡"])
 ("\\frake" ["𝔢"])
 ("\\frakf" ["𝔣"])
 ("\\frakg" ["𝔤"])
 ("\\frakh" ["𝔥"])
 ("\\fraki" ["𝔦"])
 ("\\frakj" ["𝔧"])
 ("\\frakk" ["𝔨"])
 ("\\frakl" ["𝔩"])
 ("\\frakm" ["𝔪"])
 ("\\frakn" ["𝔫"])
 ("\\frako" ["𝔬"])
 ("\\frakp" ["𝔭"])
 ("\\frakq" ["𝔮"])
 ("\\frakr" ["𝔯"])
 ("\\fraks" ["𝔰"])
 ("\\frakt" ["𝔱"])
 ("\\fraku" ["𝔲"])
 ("\\frakv" ["𝔳"])
 ("\\frakw" ["𝔴"])
 ("\\frakx" ["𝔵"])
 ("\\fraky" ["𝔶"])
 ("\\frakz" ["𝔷"])
 ("\\bdfrakA" ["𝕬"])
 ("\\bdfrakB" ["𝕭"])
 ("\\bdfrakC" ["𝕮"])
 ("\\bdfrakD" ["𝕯"])
 ("\\bdfrakE" ["𝕰"])
 ("\\bdfrakF" ["𝕱"])
 ("\\bdfrakG" ["𝕲"])
 ("\\bdfrakH" ["𝕳"])
 ("\\bdfrakI" ["𝕴"])
 ("\\bdfrakJ" ["𝕵"])
 ("\\bdfrakK" ["𝕶"])
 ("\\bdfrakL" ["𝕷"])
 ("\\bdfrakM" ["𝕸"])
 ("\\bdfrakN" ["𝕹"])
 ("\\bdfrakO" ["𝕺"])
 ("\\bdfrakP" ["𝕻"])
 ("\\bdfrakQ" ["𝕼"])
 ("\\bdfrakR" ["𝕽"])
 ("\\bdfrakS" ["𝕾"])
 ("\\bdfrakT" ["𝕿"])
 ("\\bdfrakU" ["𝖀"])
 ("\\bdfrakV" ["𝖁"])
 ("\\bdfrakW" ["𝖂"])
 ("\\bdfrakX" ["𝖃"])
 ("\\bdfrakY" ["𝖄"])
 ("\\bdfrakZ" ["𝖅"])
 ("\\bdfraka" ["𝖆"])
 ("\\bdfrakb" ["𝖇"])
 ("\\bdfrakc" ["𝖈"])
 ("\\bdfrakd" ["𝖉"])
 ("\\bdfrake" ["𝖊"])
 ("\\bdfrakf" ["𝖋"])
 ("\\bdfrakg" ["𝖌"])
 ("\\bdfrakh" ["𝖍"])
 ("\\bdfraki" ["𝖎"])
 ("\\bdfrakj" ["𝖏"])
 ("\\bdfrakk" ["𝖐"])
 ("\\bdfrakl" ["𝖑"])
 ("\\bdfrakm" ["𝖒"])
 ("\\bdfrakn" ["𝖓"])
 ("\\bdfrako" ["𝖔"])
 ("\\bdfrakp" ["𝖕"])
 ("\\bdfrakq" ["𝖖"])
 ("\\bdfrakr" ["𝖗"])
 ("\\bdfraks" ["𝖘"])
 ("\\bdfrakt" ["𝖙"])
 ("\\bdfraku" ["𝖚"])
 ("\\bdfrakv" ["𝖛"])
 ("\\bdfrakw" ["𝖜"])
 ("\\bdfrakx" ["𝖝"])
 ("\\bdfraky" ["𝖞"])
 ("\\bdfrakz" ["𝖟"])
 ("\\bbA" ["𝔸"])
 ("\\bbB" ["𝔹"])
 ("\\bbC" ["ℂ"])
 ("\\bbD" ["𝔻"])
 ("\\bbE" ["𝔼"])
 ("\\bbF" ["𝔽"])
 ("\\bbG" ["𝔾"])
 ("\\bbH" ["ℍ"])
 ("\\bbI" ["𝕀"])
 ("\\bbJ" ["𝕁"])
 ("\\bbK" ["𝕂"])
 ("\\bbL" ["𝕃"])
 ("\\bbM" ["𝕄"])
 ("\\bbN" ["ℕ"])
 ("\\bbO" ["𝕆"])
 ("\\bbP" ["ℙ"])
 ("\\bbQ" ["ℚ"])
 ("\\bbR" ["ℝ"])
 ("\\bbS" ["𝕊"])
 ("\\bbT" ["𝕋"])
 ("\\bbU" ["𝕌"])
 ("\\bbV" ["𝕍"])
 ("\\bbW" ["𝕎"])
 ("\\bbX" ["𝕏"])
 ("\\bbY" ["𝕐"])
 ("\\bbZ" ["ℤ"])
 ("\\bba" ["𝕒"])
 ("\\bbb" ["𝕓"])
 ("\\bbc" ["𝕔"])
 ("\\bbd" ["𝕕"])
 ("\\bbe" ["𝕖"])
 ("\\bbf" ["𝕗"])
 ("\\bbg" ["𝕘"])
 ("\\bbh" ["𝕙"])
 ("\\bbi" ["𝕚"])
 ("\\bbj" ["𝕛"])
 ("\\bbk" ["𝕜"])
 ("\\bbl" ["𝕝"])
 ("\\bbm" ["𝕞"])
 ("\\bbn" ["𝕟"])
 ("\\bbo" ["𝕠"])
 ("\\bbp" ["𝕡"])
 ("\\bbq" ["𝕢"])
 ("\\bbr" ["𝕣"])
 ("\\bbs" ["𝕤"])
 ("\\bbt" ["𝕥"])
 ("\\bbu" ["𝕦"])
 ("\\bbv" ["𝕧"])
 ("\\bbw" ["𝕨"])
 ("\\bbx" ["𝕩"])
 ("\\bby" ["𝕪"])
 ("\\bbz" ["𝕫"])
 ("\\_a" ["ₐ"])
 ("\\_e" ["ₑ"])
 ("\\_h" ["ₕ"])
 ("\\_i" ["ᵢ"])
 ("\\_j" ["ⱼ"])
 ("\\_k" ["ₖ"])
 ("\\_l" ["ₗ"])
 ("\\_m" ["ₘ"])
 ("\\_n" ["ₙ"])
 ("\\_o" ["ₒ"])
 ("\\_p" ["ₚ"])
 ("\\_r" ["ᵣ"])
 ("\\_s" ["ₛ"])
 ("\\_t" ["ₜ"])
 ("\\_u" ["ᵤ"])
 ("\\_v" ["ᵥ"])
 ("\\_x" ["ₓ"])
 ("\\^A" ["ᴬ"])
 ("\\^B" ["ᴮ"])
 ("\\^D" ["ᴰ"])
 ("\\^E" ["ᴱ"])
 ("\\^G" ["ᴳ"])
 ("\\^H" ["ᴴ"])
 ("\\^I" ["ᴵ"])
 ("\\^J" ["ᴶ"])
 ("\\^K" ["ᴷ"])
 ("\\^L" ["ᴸ"])
 ("\\^M" ["ᴹ"])
 ("\\^N" ["ᴺ"])
 ("\\^O" ["ᴼ"])
 ("\\^P" ["ᴾ"])
 ("\\^R" ["ᴿ"])
 ("\\^T" ["ᵀ"])
 ("\\^U" ["ᵁ"])
 ("\\^V" ["ⱽ"])
 ("\\^W" ["ᵂ"])
 ("\\^a" ["ᵃ"])
 ("\\^b" ["ᵇ"])
 ("\\^c" ["ᶜ"])
 ("\\^d" ["ᵈ"])
 ("\\^e" ["ᵉ"])
 ("\\^f" ["ᶠ"])
 ("\\^g" ["ᵍ"])
 ("\\^h" ["ʰ"])
 ("\\^i" ["ⁱ"])
 ("\\^j" ["ʲ"])
 ("\\^k" ["ᵏ"])
 ("\\^l" ["ˡ"])
 ("\\^m" ["ᵐ"])
 ("\\^n" ["ⁿ"])
 ("\\^o" ["ᵒ"])
 ("\\^p" ["ᵖ"])
 ("\\^q" ["ᶝ"])
 ("\\^r" ["ʳ"])
 ("\\^s" ["ˢ"])
 ("\\^t" ["ᵗ"])
 ("\\^u" ["ᵘ"])
 ("\\^v" ["ᵛ"])
 ("\\^w" ["ʷ"])
 ("\\^x" ["ˣ"])
 ("\\^y" ["ʸ"])
 ("\\^z" ["ᶻ"])
 ("\\^tm" ["™"])
 ("\\sca" ["ᴀ"])
 ("\\scb" ["ʙ"])
 ("\\scc" ["ᴄ"])
 ("\\scd" ["ᴅ"])
 ("\\sce" ["ᴇ"])
 ("\\scf" ["ꜰ"])
 ("\\scg" ["ɢ"])
 ("\\sch" ["ʜ"])
 ("\\sci" ["ɪ"])
 ("\\scj" ["ᴊ"])
 ("\\sck" ["ᴋ"])
 ("\\scl" ["ʟ"])
 ("\\scm" ["ᴍ"])
 ("\\scn" ["ɴ"])
 ("\\sco" ["ᴏ"])
 ("\\scp" ["ᴘ"])
 ("\\scr" ["ʀ"])
 ("\\scs" ["ꜱ"])
 ("\\sct" ["ᴛ"])
 ("\\scu" ["ᴜ"])
 ("\\scv" ["ᴠ"])
 ("\\scw" ["ᴡ"])
 ("\\scy" ["ʏ"])
 ("\\scz" ["ᴢ"])
 ("\\wc0" ["⓪"])
 ("\\wc1" ["①"])
 ("\\wc2" ["②"])
 ("\\wc3" ["③"])
 ("\\wc4" ["④"])
 ("\\wc5" ["⑤"])
 ("\\wc6" ["⑥"])
 ("\\wc7" ["⑦"])
 ("\\wc8" ["⑧"])
 ("\\wc9" ["⑨"])
 ("\\wcA" ["Ⓐ"])
 ("\\wcB" ["Ⓑ"])
 ("\\wcC" ["Ⓒ"])
 ("\\wcD" ["Ⓓ"])
 ("\\wcE" ["Ⓔ"])
 ("\\wcF" ["Ⓕ"])
 ("\\wcG" ["Ⓖ"])
 ("\\wcH" ["Ⓗ"])
 ("\\wcI" ["Ⓘ"])
 ("\\wcJ" ["Ⓙ"])
 ("\\wcK" ["Ⓚ"])
 ("\\wcL" ["Ⓛ"])
 ("\\wcM" ["Ⓜ"])
 ("\\wcN" ["Ⓝ"])
 ("\\wcO" ["Ⓞ"])
 ("\\wcP" ["Ⓟ"])
 ("\\wcQ" ["Ⓠ"])
 ("\\wcR" ["Ⓡ"])
 ("\\wcS" ["Ⓢ"])
 ("\\wcT" ["Ⓣ"])
 ("\\wcU" ["Ⓤ"])
 ("\\wcV" ["Ⓥ"])
 ("\\wcW" ["Ⓦ"])
 ("\\wcX" ["Ⓧ"])
 ("\\wcY" ["Ⓨ"])
 ("\\wcZ" ["Ⓩ"])
 ("\\wca" ["ⓐ"])
 ("\\wcb" ["ⓑ"])
 ("\\wcc" ["ⓒ"])
 ("\\wcd" ["ⓓ"])
 ("\\wce" ["ⓔ"])
 ("\\wcf" ["ⓕ"])
 ("\\wcg" ["ⓖ"])
 ("\\wch" ["ⓗ"])
 ("\\wci" ["ⓘ"])
 ("\\wcj" ["ⓙ"])
 ("\\wck" ["ⓚ"])
 ("\\wcl" ["ⓛ"])
 ("\\wcm" ["ⓜ"])
 ("\\wcn" ["ⓝ"])
 ("\\wco" ["ⓞ"])
 ("\\wcp" ["ⓟ"])
 ("\\wcq" ["ⓠ"])
 ("\\wcr" ["ⓡ"])
 ("\\wcs" ["ⓢ"])
 ("\\wct" ["ⓣ"])
 ("\\wcu" ["ⓤ"])
 ("\\wcv" ["ⓥ"])
 ("\\wcw" ["ⓦ"])
 ("\\wcx" ["ⓧ"])
 ("\\wcy" ["ⓨ"])
 ("\\wcz" ["ⓩ"])
 ("\\A`" ["À"])
 ("\\A'" ["Á"])
 ("\\E`" ["È"])
 ("\\E'" ["É"])
 ("\\a`" ["à"])
 ("\\a'" ["á"])
 ("\\e`" ["è"])
 ("\\e'" ["é"])
 ("\\o.." ["ö"])
 ("\\a.." ["ä"])
 ("\\u.." ["ü"])
 ("\\ae" ["æ"])
 ("\\oe" ["œ"])
 ("\\e^~" ["ễ"])
 ("\\u'" ["ú"])
 ("\\i.." ["ï"])
 ("\\osl" ["ø"])
 ("\\Osl" ["Ø"])
 ("\\osls" ["ᴓ"])
 ("\\boxlr" ["─"])
 ("\\boxLR" ["━"])
 ("\\boxtb" ["│"])
 ("\\boxTB" ["┃"])
 ("\\boxlr-" ["┄"])
 ("\\boxLR-" ["┅"])
 ("\\boxtb-" ["┆"])
 ("\\boxTB-" ["┇"])
 ("\\boxlr." ["┈"])
 ("\\boxLR." ["┉"])
 ("\\boxtb." ["┊"])
 ("\\boxTB." ["┋"])
 ("\\boxbr" ["┌"])
 ("\\boxbR" ["┍"])
 ("\\boxBr" ["┎"])
 ("\\boxBR" ["┏"])
 ("\\boxbl" ["┐"])
 ("\\boxbL" ["┑"])
 ("\\boxBl" ["┒"])
 ("\\boxBL" ["┓"])
 ("\\boxtr" ["└"])
 ("\\boxtR" ["┕"])
 ("\\boxTr" ["┖"])
 ("\\boxTR" ["┗"])
 ("\\boxtl" ["┘"])
 ("\\boxtL" ["┙"])
 ("\\boxTl" ["┚"])
 ("\\boxTL" ["┛"])
 ("\\boxtbr" ["├"])
 ("\\boxtbR" ["┝"])
 ("\\boxTbr" ["┞"])
 ("\\boxtBr" ["┟"])
 ("\\boxTBr" ["┠"])
 ("\\boxTbR" ["┡"])
 ("\\boxtBR" ["┢"])
 ("\\boxTBR" ["┣"])
 ("\\boxtbl" ["┤"])
 ("\\boxtbL" ["┥"])
 ("\\boxTbl" ["┦"])
 ("\\boxtBl" ["┧"])
 ("\\boxTBl" ["┨"])
 ("\\boxTbL" ["┩"])
 ("\\boxtBL" ["┪"])
 ("\\boxTBL" ["┫"])
 ("\\boxblr" ["┬"])
 ("\\boxbLr" ["┭"])
 ("\\boxblR" ["┮"])
 ("\\boxbLR" ["┯"])
 ("\\boxBlr" ["┰"])
 ("\\boxBLr" ["┱"])
 ("\\boxBlR" ["┲"])
 ("\\boxBLR" ["┳"])
 ("\\boxtlr" ["┴"])
 ("\\boxtLr" ["┵"])
 ("\\boxtlR" ["┶"])
 ("\\boxtLR" ["┷"])
 ("\\boxTlr" ["┸"])
 ("\\boxTLr" ["┹"])
 ("\\boxTlR" ["┺"])
 ("\\boxTLR" ["┻"])
 ("\\boxtblr" ["┼"])
 ("\\boxtbLr" ["┽"])
 ("\\boxtblR" ["┾"])
 ("\\boxtbLR" ["┿"])
 ("\\boxTblr" ["╀"])
 ("\\boxtBlr" ["╁"])
 ("\\boxTBlr" ["╂"])
 ("\\boxTbLr" ["╃"])
 ("\\boxTblR" ["╄"])
 ("\\boxtBLr" ["╅"])
 ("\\boxtBlR" ["╆"])
 ("\\boxTbLR" ["╇"])
 ("\\boxtBLR" ["╈"])
 ("\\boxTBLr" ["╉"])
 ("\\boxTBlR" ["╊"])
 ("\\boxTBLR" ["╋"])
 ("\\boxlr:" ["╌"])
 ("\\boxLR:" ["╍"])
 ("\\boxtb:" ["╎"])
 ("\\boxTB:" ["╏"])
 ("\\boxLR=" ["═"])
 ("\\boxTB=" ["║"])
 ("\\boxbR=" ["╒"])
 ("\\boxBr=" ["╓"])
 ("\\boxBR=" ["╔"])
 ("\\boxbL=" ["╕"])
 ("\\boxBl=" ["╖"])
 ("\\boxBL=" ["╗"])
 ("\\boxtR=" ["╘"])
 ("\\boxTr=" ["╙"])
 ("\\boxTR=" ["╚"])
 ("\\boxtL=" ["╛"])
 ("\\boxTl=" ["╜"])
 ("\\boxTL=" ["╝"])
 ("\\boxtbR=" ["╞"])
 ("\\boxTBr=" ["╟"])
 ("\\boxTBR=" ["╠"])
 ("\\boxtbL=" ["╡"])
 ("\\boxTBl=" ["╢"])
 ("\\boxTBL=" ["╣"])
 ("\\boxbLR=" ["╤"])
 ("\\boxBlr=" ["╥"])
 ("\\boxBLR=" ["╦"])
 ("\\boxtLR=" ["╧"])
 ("\\boxTlr=" ["╨"])
 ("\\boxTLR=" ["╩"])
 ("\\boxtbLR=" ["╪"])
 ("\\boxTBlr=" ["╫"])
 ("\\boxTBLR=" ["╬"])
 ("\\boxbrc" ["╭"])
 ("\\boxblc" ["╮"])
 ("\\boxtlc" ["╯"])
 ("\\boxtrc" ["╰"])
 ("\\boxtrbl" ["╱"])
 ("\\boxtlbr" ["╲"])
 ("\\boxx" ["╳"])
 ("\\boxl" ["╴"])
 ("\\boxt" ["╵"])
 ("\\boxr" ["╶"])
 ("\\boxb" ["╷"])
 ("\\boxL" ["╸"])
 ("\\boxT" ["╹"])
 ("\\boxR" ["╺"])
 ("\\boxB" ["╻"])
 ("\\boxlR" ["╼"])
 ("\\boxtB" ["╽"])
 ("\\boxLr" ["╾"])
 ("\\boxTb" ["╿"])
 ("\\nbsp" [" "])
 ("\\ensp" [" "])
 ("\\fgsp" [" "])
)
// Get Polylux from the official package repository
#import "@preview/polylux:0.3.1": *
#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import themes.simple: *
#import fletcher.shapes: diamond


// Make the paper dimensions fit for a presentation and the text larger
#set page(paper: "presentation-16-9")
#set footnote.entry(clearance: 0.1em, gap: 0.2em)
#show heading:set align(start + top)
#show heading: set text(size: 28pt)
#set align(horizon)

#let lb = linebreak(justify: false)

#set list(marker: ([•], [◦], [🗸]))
#let list-counter = counter("list")

#show list: set text(14pt)
#show list: it => {
  list-counter.step()

  context {
    set text(24pt) if list-counter.get().first() == 1
    set text(20pt) if list-counter.get().first() == 2
    set text(16pt) if list-counter.get().first() >= 3
    it
  }
  list-counter.update(i => i - 1)
}

// for main text
#set text(
    lang: "ja",  // 英語しか使わない文書では"en"とする（もしくは指定しない）
    font: ("Calibri", "Noto Serif CJK JP"),  
    // font: (日本語文字を含まないフォント, 日本語文字を含むフォント),  となっている
)

// Use #polylux-slide to create a slide and style it using your favourite Typst functions

#polylux-slide[

#align(horizon + center)[
  #text(size: 36pt, weight: "bold")[
    Systemic amyloidosisとは
  ]
  
  #text(size: 24pt)[
    Nozomi Niimi #lb
    2024/12/4
  ]

]
]

#polylux-slide[
    == Amyloidとは？
 - 1854年にVirchowが発見したミスフォールド前駆体 #lb タンパク質に由来する線維状物質
  #align(right)[
   #text(size: 12pt)[
    Amyloid. 2020;27(4):217-222.
   
   DynaMed. Amyloidosis. EBSCO Information Services. Accessed September 16th, 2024]
  ] 
 - 全身の臓器に沈着し、多くの臓器障害を起こす
 - 有病率はだいたい100万人あたり14人と推定されている
   - CMLとかと同じくらいの頻度

]

#polylux-slide[
  == Amyloidosisの分類

 - 遺伝性 vs. 後天性 / 全身性 vs. 局所
   - 局所の代表例はCAAとか、Alzheimer型認知症とか #lb → 今回は*全身性のアミロイドーシス*に焦点を当てる
 - Amyloidの種類によっても分類可能
   - AA, AL, ATTR etcetc
 - Amyloidの種類での分類が最もわかりやすい

]

#polylux-slide[
  == Amyloidの種類の重要性

 - 全身性/局所性も遺伝性/後天性もAmyloidの種類で #lb ある程度わかる
 - 障害される臓器PatternもAmyloidの種類でわかる事が多い
 - 何よりも、#text(fill: red)[*治療法の有無*]が決定される(後述)

]



#polylux-slide[
  == Amyloidの種類と特徴

  #align(center)[
   #text(size: 28pt, fill: red)[*最重要スライド！！*]
  ] 
#set text(size: 16pt)
#table(
  columns: 5,
  [*種類*], [*前駆物質*], [*遺伝性/後天性*], [*障害臓器*], [*全身性/局所性*],
  [AL], [免疫グロブリン軽鎖],[両方], [全臓器、中枢神経は稀], [両方],
  [AA], [血清アミロイドA],[後天性], [中枢神経以外全て、通常腎臓], [全身性],
  [ATTR-wt], [トランスサイレチン],[後天性], [心臓、肺、腱], [全身性],
  [ATTR-v], [トランスサイレチン], [遺伝性], [末梢/自律神経、心臓、目、髄膜], [全身性]
)
  #align(right)[
   #text(size: 12pt)[DynaMed. Amyloidosis. EBSCO Information Services. Accessed September 16th, 2024]
  ] 
#set text(size: 20pt)
  - 全部で30種類以上の前駆物質が判明しているが、上記の4種類でだいたい全部のうち80%くらいは占めている
  
]

#polylux-slide[
  == Amyloidosisの疫学

  - Underdiagnosisされているので正確な疫学は不明
  - イギリスのAmyloidosisセンターの1990-2014年までの疫学研究
  - AL: 60%, AA: 10.5%, ATTR-wt: 8%, ATTR-v: 10%
  - ATTRは今後増えるかもしれない
#align(right)[
   #text(size: 12pt)[Amyloid. 2017 Sep;24(3):162-166.]
  ] 
]

#polylux-slide[
  == 全身性Amyloidosisの診断は難しい！

  - 症状は非特異的な事が多い
  - そのため、発症から診断までに時間がかかることが知られている
    + 診断までの中央値は7ヶ月
    + 4割の患者が1年以上、10%以上の患者が3年以上経過して診断される
#align(right)[
   #text(size: 12pt)[
    Acta Haematol. 2020;143(4):304-311.  
]
  ] 
]


#polylux-slide[
  == どうやって診断する？

  - 代表的な症状は倦怠感、低栄養であり、そこから攻めるのは辛い
  - どちらかというと、#text(size: 24pt, fill: red)[*特定の臓器障害*]を見て疑った後にRed flagを探すのが現実的
]

#polylux-slide[
  == Amyloidosisの診断

  #diagram(
    node-stroke: 1pt, 
    edge-stroke: 1pt,
    node((0, 0), [#text(size: 20pt)[Amyloidosisを疑う]], corner-radius: 2pt),
    edge("-|>"), 
    node((1.0, 0), [#text(size: 20pt)[Clueとなる追加情報\ を集める]], corner-radius: 2pt), 
    edge("-|>"),
    node((2.0, 0), [#text(size: 20pt)[AmyloidosisのTyping:\ 侵襲的検査]], corner-radius: 2pt)
    )
  
  - Step by stepで考える
  - 特徴的な臓器障害と違和感を見逃さない

]

#polylux-slide[
  == 特徴的な臓器障害

  - Amyloidが沈着しやすい臓器が決まっており、以下の時にSystemic amyloidosisを疑う
    + 非糖尿病患者のネフローゼ症候群
    + HFpEF(特に、強いLVHを伴う)
    + 肝脾腫
    + Gloves and stockings patternのPolyneuropathy
    + MGUS患者の消耗性の症状

#align(right)[
   #text(size: 12pt)[
    JAMA. 2020;324(1):79-89. 
]
  ] 
]

#polylux-slide[
  == ゲシュタルトを覚えよう

  - 出来れば、AA, AL, ATTR-wtの3つはゲシュタルトを #lb 覚えておくと良い
  - 疑った時に問診・身体所見を追加

  
]

#polylux-slide[
  == AA amyloidosisの疾患シナリオ

  - 年齢の中央値は50-60歳
  - 慢性炎症性疾患の背景がある患者の高度蛋白尿、全身浮腫
    - TB, RA, IBD, SLE, FMF, Sarcoidosis, HIVなど
    - 蛋白尿が95%でNephrosis rangeは50%にもなる
  - 肝脾腫は10%程度
  - 心不全や神経障害は非典型的
  #align(right)[
   #text(size: 12pt)[Rheum Dis Clin North Am. 2018;44(4):585-603.]
  ] 
]

#polylux-slide[
  == AL amyloidosisの疾患シナリオ①

#side-by-side(gutter: 1mm, columns: (1.5fr, 1fr))[
  - 診断時の年齢は50-70歳が殆ど
  - 以下の症状の時に巨舌(10-17%)や眼窩周囲の紫斑(15%)を探す
    - 心不全入院: 特にLVHを伴うHFpEF (60-75%)
    - 腎機能低下: 著名な蛋白尿 (50-70%)
    - 神経障害: 両手足のしびれ、#lb 両側の手根管症候群、 #lb 起立性低血圧による失神・めまい (22%)

    // + 巨舌: 10-17%
    // + 眼窩周囲の紫斑: 15%
    // + 心疾患: 60-75%
    // + 腎疾患: 50-70%
    // + 神経: 22%
    // + 肝臓: 20%
    // + 腸管: 10-17%

  #align(right)[
   #text(size: 12pt)[DynaMed. Amyloidosis. EBSCO Information Services. #lb Accessed September 16th, 2024]
  ] 

][
  #figure(image("figures/al_amyloidosis_gestalt.png", height: 80%))
  #align(right)[
   #text(size: 12pt)[
    N Engl J Med. 2024;390(24):2295-2307.]
  ] 
]
]

#polylux-slide[
  == AL amyloidosisの疾患シナリオ②

  - 元々MGUSなどの基礎疾患がわかっている患者が、倦怠感や #lb 浮腫、体重減少などの非特異的な症状で来院
  - 検査で心不全や腎機能低下、臓器腫大が判明
  
  #align(right)[
   #text(size: 12pt)[JAMA. 2020;324(1):79-89.]
  ] 
]

#polylux-slide[
  == ATTR amyloidosisの疾患シナリオ

  - 年齢の中央値は75歳, 90%は男性
  - 高齢者のHFpEFでエコーをしたら特徴的な所見
    - 後壁の心室壁厚 > 15mm, Granular sparkiling pattern、ECGで低電位など
  - 神経: 手根管症候群が30-50%、脊柱管狭窄症、DSP
  - 高齢者の上腕二頭筋腱断裂, 両側手根管症候群のような #lb やや違和感があるStory
  - 腎疾患は稀

  #align(right)[
   #text(size: 12pt)[Rheum Dis Clin North Am. 2018;44(4):585-603.]
  ] 
]

#polylux-slide[
  == Amyloidosisの疾患シナリオ

  - 腎臓のNephrosis → AA, AL amyloidosis
  - 心Amyloidosis → AL, ATTR amyloidosis
  - Polyneuropathy → AL, ATTR amyloidosis

  #align(center)[#text(size: 32pt, fill: red, weight: "bold")[*最も重要なのは心Amyloidosis*]]
  
]


#polylux-slide[
  == 心Amyloidosisの重要性
  
  - 心不全は#text(size: 24pt, fill: red)[*最も重要な合併症かつ、予後規定因子*]
  - HFpEFの中でも隠れ心amyloidosisが多いとされる
#align(right)[
   #text(size: 12pt)[ESC Heart Fail. 2023;10(3):1896-1906.]
] 
  - とはいえ、HFpEF全例で疑うのはやはり現実的ではない
    - 病歴で、Polyneuropathy、手根管症候群、Nephrosisの合併
  - 心電図も疑うきっかけになる

]


#polylux-slide[
  == ECGの特徴

  #side-by-side(gutter: 1mm, columns: (1fr, 1fr))[
    #figure(image("figures/amyloid_ecg.jpeg", width: 100%))
  ][
    #figure(image("figures/ecg_table.jpeg", width: 100%))
  ]
  
  - AF
  - 壁肥厚があるにもかかわらず、心電図が低電位
  - QS pattern(偽梗塞パターン)

]

#polylux-slide[
  == 心Amyloidosisの診断
  
  - 侵襲性と値段の問題から、まずはTTEが使われる事が多い
  - CMR(当院でややハードルは高いが)もかなり精度が良い
  - TTE→CMR→更なる検査という順番が現実的
  - CMRはCVICという会社でもやってくれる(飯田橋だがオススメ)

]


#slide[
  
    #side-by-side(gutter: 1mm, columns: (1.2fr, 1fr))[

    == TTEの特徴

  - 心筋壁/乳頭筋/弁/中隔の肥厚  
  - 中隔/後壁比が< 1.3，
  - 肥厚心筋の “granular sparkling”
  - 心室中隔/左室後壁の低収縮
  - 心電図上の低電位と心室中隔肥厚 #lb (> 1.98 cm)の組み合わせで， #lb 感度 72%，特異度 91%
    
  ][
    #figure(image("figures/amyloid_tte.jpeg", height: 60%))
  ]
  
  
]


#polylux-slide[
  == CMRの特徴
  #side-by-side(gutter: 1mm, columns: (1fr, 1fr))[
    #figure(image("figures/cmr_lge.jpeg", height: 40%))
  ][
    #figure(image("figures/cmr_t1mapping.jpeg", height: 40%))
  ]
  
  - CMR: 基本は造影MRI
    - LGEは左心室内膜下優位に分布(感度85%, 特異度92%)
  - 造影剤が使えない時もT1 mappingという手法で診断可能
    - 感度80-92%，特異度56-91%
    - 当院だとできないらしい・・・・・・

]

#polylux-slide[
  == 心Amyloidosisのタイピング
  
  - AL, ATTR amyloidosisの診断をする(95％がこの2種類)
  - AL: Monoclonal蛋白検出で非侵襲的に診断可能 
    - 血液・尿中免疫電気泳動/固定法、Free Light Chain
    - これらが全て陰性の時の感度は約99%
    #align(right)[
   #text(size: 12pt)[JAMA. 2024;331(9):778-791.]
] 
  - ATTR: *PyPシンチ(Tc99m)*を使う事で、非侵襲的に診断可能
    - Monoclonal蛋白陰性→PyPシンチ陽性 = ATTR amyloidosis確定診断

]


#polylux-slide[
  == PyPシンチ

  #side-by-side(gutter: 1mm, columns: (1.4fr, 1fr))[
  
  - 陽性の場合はATTR amyloidosisを示唆する
  - ATTR-CAの診断能は #lb 感度58-99%，特異度79-100%
  - AL amyloidosisでも陽性になるので注意！
    - 採血・検尿でALを除外したら #lb 一発診断可能
  ][
    #figure(image("figures/pyp_scinti.jpeg", height: 80%))

]
]

#polylux-slide[
  == Cardiac amyloidosis(CA)診断のアルゴリズム

    #diagram(
    node-stroke: 1pt, 
    edge-stroke: 1pt,
    node((0, 0), [#text(size: 12pt)[心アミロイドーシス疑い]], corner-radius: 2pt),
    edge((0, 0), (1.0, 0), "-|>", [#text(size: 12pt)[検査]]), 
    node((1.0, 0), [#text(size: 12pt)[monoclonal蛋白検査]], corner-radius: 2pt), 
    edge((1.0, 0), (1.0, 0.6), "-|>", [#text(size: 12pt)[陽性]]),
    node((1.0, 0.6), [#text(size: 12pt)[血液内科コンサルト・生検]]),
    edge((1.0, 0), (2.0, 0),"-|>", [#text(size: 12pt)[陰性]]),
    node((2.0, 0), [#text(size: 16pt)[PyPシンチ]], corner-radius: 2pt),
    edge((2.0, 0), (3.0, 0), "-|>", [#text(size: 12pt)[陰性]]),
    node((3.0, 0), [#text(size: 16pt)[心アミロイドーシス\ らしくない]], corner-radius: 2pt), 
    edge((2.0, 0), (2.0, 0.6), "-|>", [#text(size: 12pt)[陽性]]),
    node((2.0, 0.6), [#text(size: 16pt)[ATTR-CM]], corner-radius: 2pt)
    )
  
  #text(size: 20pt)[

  - 採血・検尿が陰性→PyPシンチ陽性→ATTR amyloidosis確定
  - 採血・検尿でM蛋白が陽性→Amyloidの組織生検は必須
    - MGUSは70歳以上で5%、ATTR-wtのCAのうち10-40%は #lb AL amyloidosisの検査で異常が出る為
    - 局所麻酔下での脂肪織の生検が非侵襲的でよい
      - 感度 ATTR-v: 65-85%, AL: 60-80%, ATTR-wt:14%
      ]

    #align(right)[
   #text(size: 12pt)[JAMA. 2024;331(9):778-791.]
] 



]
#polylux-slide[
  == Cardiac amyloidosis(CA)診断のまとめ

#figure(image("figures/ca_pathway.png", height: 80%))

 #align(right)[
   #text(size: 12pt)[Ann Intern Med. 2023;176(3):ITC33-ITC48.   
]
] 

]

#polylux-slide[
  == Amyloidosisの診断後

  - 治療は専門科に任せる  
  - AL amyloidosisは血液内科
  - ATTRは慶應の循環器内科に紹介
    - Tafosmideをやっている
  - ATTRがwtかv(遺伝性か孤発性か)は家族歴を確認するが結局は遺伝子検査が必須
    - 熊本と長野に集積

]

#polylux-slide[
  == Take home message
  
  - 特徴的な臓器障害パターンからAmyloidosisを引っ掛けよう
  - 心アミロイドーシスが最も重要！探しに行く！
  - ALは採血・検尿、ATTR-wtはPypシンチで非侵襲的に診断を！
  - 最終的にはTissue is issue！ATTR-wtアミロイドーシス疑いの時は腹壁脂肪を生検する！
  
]

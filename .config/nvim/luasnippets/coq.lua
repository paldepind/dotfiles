function tex(name, thing)
  return s({ trig = "\\" .. name, snippetType = "autosnippet" }, thing)
end

function texs(name, sym)
  return tex(name, { t(sym) })
end

return {
  s(
    { trig="Lem", dscr="A Coq lemma", snippetType="autosnippet" },
    fmta(
      [[
        Lemma <> :
          <>.
        Proof.
          <>
        Qed.
      ]],
      { i(1), i(2), i(0) }
    )
  ),
  s(
    { trig = "Sec", snippetType = "autosnippet" },
    fmt(
      [[
        Section {}.
          {}
        End {}.
      ]],
      { i(1), i(0), rep(1) }
    )
  ),
  -- Unicode
  s({ trig = "\\wand", snippetType = "autosnippet" }, { t("-∗") }),
  s({ trig = "\\lc", snippetType = "autosnippet" }, { t("⌜ "), i(1), t(" ⌝") }),
  s({ trig = "\\rc", snippetType = "autosnippet" }, { t("⌝") }),
  s({ trig = "\\Lc", snippetType = "autosnippet" }, { t("⎡ "), i(1), t(" ⎤") }),
  s({ trig = "\\Rc", snippetType = "autosnippet" }, { t("⎤") }),
  texs("sub", "⊆"),
  texs("sqsub", "⊑"),
  texs("upd", "⇝"),
  texs("Phi"   , "Φ"),
  texs("fun"   , "λ"),
  texs("mult"  , "⋅"),
  texs("ent"   , "⊢"),
  texs("valid" , "✓"),
  texs("diamond", "◇"),
  texs("box"   , "□"),
  texs("bbox"  , "■"),
  texs("later" , "▷"),
  texs("unit"  , "ε"),
  texs("phi", "φ"),
  texs("and"   , "∧"),
  texs("or"    , "∨"),
  texs("comp"  , "∘"),
  texs("ccomp" , "◎"),
  texs("all"   , "∀"),
  texs("ex"    , "∃"),
  texs("to"    , "→"),
  texs("sep"   , "∗"),
  texs("col"   , "∷"),
  texs("lam"   , "λ"),
  texs("emp" , "∅"),
  texs("Lam"   , "Λ"),
  texs("Sig"   , "Σ"),
  texs("sig"   , "σ"),
  texs("-"     , "∖"),
  texs("aa"    , "●"),
  texs("af"    , "◯"),
  texs("auth"  , "●"),
  texs("frag"  , "◯"),
  texs("iff"   , "↔"),
  texs("gn",     "γ"),
  texs("incl"  , "≼"),
  texs("in ", "∈ "),
  texs("latert", "▶"),
  texs("light", "⚡"),
  texs("ng", "⚡==>"),
}

let
  layout = ''
    default  partial alphanumeric_keys
    xkb_symbols "basic" {
        include "gb(dvorak)"
        name[Group1]= "Breaded Dvorak";
    
        key <AE01>	{ [         1,     exclam,  onesuperior,   exclamdown ]	};
        key <AE02>	{ [         2,   quotedbl,  twosuperior,      onehalf ]	};
        key <AE03>	{ [         3,   sterling, threesuperior,    onethird ]	};
        key <AE04>	{ [         4,     dollar,     EuroSign,   onequarter ]	};
        key <AE05>	{ [         5,    percent,    permille, threequarters ]	};
        key <AE06>	{ [         6, asciicircum,   copyright,   registered ] };
        key <AE07>	{ [         7,  ampersand,     currency,         cent ]	};
        key <AE08>	{ [         8,   asterisk,          yen,    paragraph ]	};
        key <AE09>	{ [         9,  parenleft,     multiply,     division ]	};
        key <AE10>	{ [         0, parenright,       dagger, doubledagger ]	};
        key <AE11>	{ [ bracketleft, braceleft, guillemotleft, leftdoublequotemark ]	};
        key <AE12>	{ [ bracketright, braceright, guillemotright, rightdoublequotemark ]	};
    
        key <AD01>	{ [apostrophe,      at, dead_abovedot, dead_diaeresis ]	};
        key <AD02>	{ [     comma,      less, dead_caron, dead_circumflex ]	};
        key <AD03>	{ [    period,  greater, dead_acute, dead_doubleacute ]	};
        key <AD04>	{ [         p,        P, dead_grave, dead_doublegrave ]	};
        key <AD05>	{ [         y,      Y, dead_breve, dead_invertedbreve ]	};
        key <AD06>	{ [         f,      F, dead_abovering, dead_belowring ]	};
        key <AD07>	{ [         g,          G,     dead_psili, dead_dasia ]	};
        key <AD08>	{ [         c,          C,   dead_caron, dead_cedilla ]	};
        key <AD09>	{ [         r,          R,  Greek_sigma,  Greek_SIGMA ]	};
        key <AD10>	{ [         l,          L, Greek_lambda, Greek_LAMBDA ]	};
        key <AD11>	{ [     slash,   question, questiondown,        U203D ] };
        key <AD12>	{ [     equal,       plus,    plusminus,        U262D ]	};
    
        key <AC01>	{ [         a,          A,   adiaeresis,   Adiaeresis ]	};
        key <AC02>	{ [         o,          O,   odiaeresis,   Odiaeresis ]	};
        key <AC03>	{ [         e,          E,           ae,           AE ]	};
        key <AC04>	{ [         u,          U,   udiaeresis,   Udiaeresis ]	};
        key <AC05>	{ [         i,          I,       oslash,       Oslash ]	};
        key <AC06>	{ [         d,          D,          eth,          ETH ]	};
        key <AC07>	{ [         h,          H,        aring,        Aring ] };
        key <AC08>	{ [         t,          T,        thorn,        THORN ]	};
        key <AC09>	{ [         n,          N,    Greek_tau,    Greek_PSI ]	};
        key <AC10>	{ [         s,          S,       ssharp,        U1E9E ]	};
        key <AC11>	{ [    minus,  underscore,    Greek_rho,    Greek_psi ]	};
        key <TLDE>      { [	grave,    notsign,          bar, femalesymbol ] };
        key <LSGT>	{ [ backslash,        bar,  dead_macron,   malesymbol ]	}; 
    
        key <BKSL>	{ [numbersign, asciitilde,      Greek_pi,    Greek_PI ]	};
        key <AB01>	{ [ semicolon,      colon,        degree,  dead_tilde ]	};
        key <AB02>	{ [         q,          Q,   Greek_gamma, Greek_GAMMA ]	};
        key <AB03>	{ [         j,          J,   Greek_alpha,   trademark ]	};
        key <AB04>	{ [         k,          K,   Greek_kappa, Greek_theta ]	};
        key <AB05>	{ [         x,          X,     Greek_chi, Greek_THETA ] };
        key <AB06>	{ [         b,          B,   Greek_beta,Greek_epsilon ]	};
        key <AB07>	{ [         m,          M,      Greek_mu,   Greek_phi ]	};
        key <AB08>	{ [         w,          W,   Greek_omega, Greek_OMEGA ]	};
        key <AB09>	{ [         v,          V,   Greek_delta, Greek_DELTA ]	};
        key <AB10>	{ [         z,          Z,    Greek_zeta,   Greek_PHI ] };
    
    };
    // key <BKSL>       { [ numbersign,     asciitilde	  ] };
    // key <BKSL>	{ [ backslash bar,  dead_macron,  dead_belowmacron  ] }; 
    // key <TLDE>	{ [ numbersign,     asciitilde,   Greek_pi,         Greek_PI ] };
    // See /usr/include/X11/keysymdef.h for Symbol explanation
  '';
    
  guide = ''
    ┌─────┐
    │ 2 4 │   2 = Shift,  4 = Level3 + Shift
    │ 1 3 │   1 = Normal, 3 = Level3
    └─────┘
    ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┲━━━━━━━━━┓
    │ ¬ ♀ │ ! ¡ │ " ½ │ £ ⅓ │ $ ¼ │ % ¾ │ ^ ® │ & ¢ │ * ¶ │ ( ÷ │ ) ‡ │ { “ │ } ” ┃ ⌫ Back- ┃
    │ ` | │ 1   │ 2 ² │ 3 ³ │ 4 € │ 5   │ 6 © │ 7 ¤ │ 8 ¥ │ 9 × │ 0 † │ [ « │ ] » ┃  space  ┃
    ┢━━━━━┷━┱───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┴─┬───┺━┳━━━━━━━┫
    ┃       ┃ @ ¨ │ < ˆ │ > ˝ │ P  ̏ │ Y ˘ │ F ˚ │ G ῾ │ C ¸ │ R Σ │ L Λ │ ? ‽ │ + ☭ ┃ Enter ┃
    ┃Tab ↹  ┃ ' · │ ,   │ . ´ │ p ` │ y  ̑ │ f ˳ │ g ᾿ │ c ˇ │ r σ │ l λ │ / ¿ │ = ± ┃   ⏎   ┃
    ┣━━━━━━━┻┱────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┴┬────┺┓      ┃
    ┃        ┃ A Ä │ O Ö │ E Æ │ U Ü │ I Ø │ D Ð │ H Å │ T Þ │ N Ψ │ S ẞ │ _ ψ │ ~ Π ┃      ┃
    ┃Caps ⇬  ┃ a ä │ o ö │ e æ │ u ü │ i ø │ d ð │ h å │ t þ │ n τ │ s ß │ - ρ │ # π ┃      ┃
    ┣━━━━━━━┳┹────┬┴────┬┴────┬┴────┬┴────┬┴────┬┴────┬┴────┬┴────┬┴────┬┴────┲┷━━━━━┻━━━━━━┫
    ┃       ┃ \ ♂ │ : ~ │ Q Γ │ J ™ │ K θ │ X Θ │ B ε │ M φ │ W Ω │ V Δ │ Z Φ ┃             ┃
    ┃Shift⇧ ┃ | ¯ │ ; ° │ q γ │ j α │ k κ │ x χ │ b β │ m μ │ w ω │ v δ │ z ζ ┃Shift ⇧      ┃
    ┣━━━━━━━╋━━━━━┷━┳━━━┷━━━┱─┴─────┴─────┴─────┴─────┴─────┴───┲━┷━━━━━╈━━━━━┻━┳━━━━━━━┳━━━┛
    ┃       ┃       ┃       ┃ ␣                               ⍽ ┃       ┃       ┃       ┃
    ┃Ctrl   ┃Meta   ┃Alt    ┃ ␣           Space               ⍽ ┃AltGr ⇮┃Menu   ┃Ctrl   ┃
    ┗━━━━━━━┻━━━━━━━┻━━━━━━━┹───────────────────────────────────┺━━━━━━━┻━━━━━━━┻━━━━━━━┛
  '';
in layout

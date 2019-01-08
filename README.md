Dvorak for (ISO) multi-linguals
===============================

Description
-----------

This is a modified uk-dvorak layout for x.org's xkb which includes a bunch of extra characters:

- ÄÖÜÆØÅÐÞ for typing Scandinavian/Germanic languages, or other languages that use them.
- A lot of combining diacritics for creating even more letters.
- A series of greek characters for mathematical equations and similar things
- Various other symbols like ©®¤¢¥¶×÷†‡±☭¡¹²³¼¾° for various other uses.

This is a work in progress, and will probably remain a WIP for pretty much ever.

Why
---

I speak several languages and there isn't one good keyboard layout to combine
my need for typing in all of these languages, while also using dvorak. 
Some languages allow replacing characters (for example, german ö can be written
oe when no ö is available), but not all of them (for instance finnish äijä
cannot be spelled aeijae.). Further typing some names of people or places
without the correct letters is a bit of a hack, and properly writing *Rhône*,
*Eyjafjallajökull*, *Tromsø*, *Österreich* or *Säynäjä* is better.

Full Layout
-----------

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

<!-- In this repository the table is Table_S3.tsv: the same bytes as Supplement_File_S2.tsv, sha256 0e621b2132f08577b5f8393be6284a46987df702e6e468485722b1078fa00191. The provenance files (S2_provenance.tsv, S2_nonlatin_rivers.tsv, S2_review.tsv, S2_mainbas_exceptions.tsv) accompany the manuscript supplement, not the repository. -->

# Supplement File S2 — hydronym lookup table

`Supplement_File_S2.tsv` is cheCkOVER's hydronym lookup table: it links every HydroBASINS unit identifier
(standard v1c, levels 6, 8 and 10; 1,148,084 rows) to its hydronyms.
Header: `Basin_level	HYBAS_ID	Basin_name	Subbasin_name	river_name`.
sha256 `0e621b2132f08577b5f8393be6284a46987df702e6e468485722b1078fa00191` (`Supplement_File_S2.tsv.sha256`).

The table carries no version of its own. It is identified by its name and by its sha256, and the
revision manifest of cheCkOVER records which table a revision was built with.

## How the table was completed

The existing names were kept wherever they are real names. New values were added component by component
(`Basin_name`, `Subbasin_name`, `river_name`), only where the existing value was blank, "unnamed", or a sea,
ocean, gulf or bay placeholder. Every value is in Latin letters.

| Rule | Component | Values |
|---|---|---|
| Existing value kept as it is | all | 2,600,956 |
| A placeholder system replaced by the table's own name for that river network (MAIN_BAS) | Basin_name | 116,421 |
| A placeholder system replaced by the new naming, where the table names the network nowhere: one name per network, the one most of its basins carry | Basin_name | 87,404 |
| A placeholder system of a network of **one basin**, which the table names nowhere: that basin's own river (Supsa, not "Sepa") | Basin_name | 12,521 |
| A blank tributary filled, only where the row's system is the one the new naming was reckoned against | Subbasin_name | 217,366 |
| A blank river filled | river_name | 252,211 |
| A river name with no Latin letter: the new naming's name, where it is **the same river** | river_name | 50,547 |
| A river name with no Latin letter: **its own river, written in Latin** | river_name | 104,277 |
| A Japanese river name with kanji and no reading found: **left blank** (a Chinese reading would be wrong) | river_name | 241 |
| A bilingual river name (Latin and another script) reduced to its Latin part | river_name | 2,265 |
| A "?" standing for lost characters: replaced by the same river from the new naming | river_name | 1 |
| A "?" standing for lost characters: removed, the name kept | river_name | 42 |
| Look-alike letters put back in their word's script ("Cредняя" → "Средняя") | river_name | 59 |
| Punctuation of other scripts made plain: ？ a lost letter like "?", ，（） as , ( ), Tibetan ། ་, Armenian ֊ | all | 346 |
| Unicode NFC | all | 42 |

All names carry single spaces and Unicode NFC. A name counts as Latin when every letter in it is of the Latin
script: punctuation and script-less marks (‘ ’ ʻ –) do not count, ə, ɣ, ḏ are Latin letters, and a Roman
numeral is no letter ("Росава II" has no Latin letter). A Cyrillic or Greek word with a Latin look-alike
inside ("Νέδoντας", "Ciлетi") gets the letter back in its own script first (and a Latin word with a Cyrillic
look-alike the other way, "Кāео River" → "Kāeo River").

Punctuation of other scripts is made plain (346 values, 17 names): a full-width ？ marks a lost letter like "?" and
is removed the same way ("枫林沙河？" → "Fenglinsha He"); the full-width ，（） are , ( ); the Tibetan marks ། and ་
become " - " where they separate two forms of a name ("Rma Chu - Ma Qu") and go elsewhere; the Armenian hyphen
֊ is "-" ("Seliv-Mastara kanal"). No value holds a character outside the Latin, Common and Inherited scripts,
or one in U+3000–303F or U+FF00–FFEF.

### A river name with no Latin letter

1. **The same river.** The new naming's Latin name is taken only where it is the same river: the Wikidata item
   of that name carries the table's name among its labels or aliases, in any language (43,967), or a GeoNames
   stream or wadi that carries the table's name is that Wikidata item or carries the new name among its Latin
   names (6,578). Names are compared after NFC, small letters, ё as е, Arabic vowel marks and letter variants
   folded, and a bracketed qualifier and the word for "river" left out.
2. **Otherwise its own river, written in Latin.** BGN/PCGN where ICU has it (ICU 77.1): Russian, Ukrainian,
   Belarusian, Bulgarian, Serbian, Macedonian, Kazakh, Kyrgyz, Mongolian, Uzbek, Azerbaijani, Turkmen,
   Georgian, Armenian, Greek, Arabic, Persian, Pashto, Hebrew, Korean, Amharic; Hanyu Pinyin for Chinese
   (syllables joined, no tone marks, the generic word apart: 大石河 → Dashi He); ICU Any-Latin otherwise.
   The language is read from the letters where they tell it (ї є ґ Ukrainian, ў Belarusian; the Pashto,
   Kurdish and Uyghur letters) and otherwise from the country the basin's own river lies in (its reach with
   the largest upstream area, in the GISCO country polygons).
   - **Japanese.** No transform reads kanji the Japanese way (ICU would write 吉野川 in Chinese). A Japanese name
     is written as Wikidata and GeoNames write a Japanese river of that name (吉野川 → Yoshino, 石狩川 →
     Ishikari): 1,159 rows. Where no such river was found, a name in kana is written in Hepburn, a closing 川
     as Gawa ("ヤウシュベツ川" → "Yaushubetsu Gawa"; 14 rows), and a name with kanji is left blank (241 rows:
     臼別川 is the Usubetsu, and Pinyin would call it "Jiubie"); its label falls back to tributary and system.
   - **Arabic, Persian and Hebrew** are written without their vowels, so ICU's BGN gives their consonants only
     ("وادي أبو درج" → "Wạdy Bw Drj"). Where GeoNames romanizes the same name in the same country (BGN/PCGN,
     with its vowels), that form is taken: "Wādī Abū Daraj" (4,534 rows); only when, the word for river,
     wadi or stream left out, its consonants are the S2 name's (edit distance over the longer length at most
     0.5). The other 10,774 keep ICU's consonants.
   - **Scripts ICU cannot write.** ICU has no Lao, Khmer, Tibetan, Mongolian, Tifinagh, N'Ko, Shan, Kurdish or
     Uyghur transform. These go through our letter tables after the usual systems: Lao BGN/PCGN (777), Khmer
     UNGEGN (440), Tibetan Wylie (556, most beside Chinese), Uyghur/Kyrgyz ULY (143), Kurdish Hawar (100),
     Shan (62), Tifinagh IRCAM and N'Ko (2). They are simplified, letter by letter: syllables the script
     runs together are written apart, since no word list tells where a word ends.
   - **A name in two scripts** is written part by part, each in its own system ("Έβρος - Марица" → "Évros -
     Maritsa", "鸭绿江 압록강" → "Yalü Jiang Abloggang"). Mongolian and Tifinagh letters beside Cyrillic or
     Arabic repeat the name and are left out ("Сэлэнгэ гол ᠰᠡᠯᠡᠩᠭᠡ ᠭᠣᠤᠯ" → "Selenge gol").
3. **The record.** `S2_nonlatin_rivers.tsv` lists every such row: the table's name, the result, the rule
   (`same river` / `transliterated` / `left blank`) and its evidence (the Wikidata item or GeoNames id; the
   transform). `S2_provenance.tsv` marks them `new-same-river`, `S2-transliterated` and `S2-no-reading`.

**Check.** Both sides through ICU `Any-Latin; Latin-ASCII`, small letters, letters only; the share whose edit
distance over the longer length is above 0.6:
- written by ICU, where Any-Latin can read the name: 15 of 96,492 (short names, where BGN and Any-Latin write
  a letter differently: Гнилиця → Hnylytsya, Яй → Yay);
- the GeoNames BGN/PCGN forms: 614 of 4,534 (their vowels, and "Rūdkhāneh-ye" for رود, are not in Any-Latin's
  consonants);
- the Japanese readings: 1,125 of 1,159 (Any-Latin reads kanji in Chinese; not a measure here); the 14 in
  Hepburn: 0; the 241 left blank are not measured;
- through our tables, where Any-Latin cannot read the name, or only part of it: 1,302 of 2,078 (not a
  measure here either);
- the same river: 5,316 of 50,547 (10.5%): a name recorded on the same Wikidata item or GeoNames feature
  that is another name of the river (نهر النيل → Nile, Південний Буг → Southern Bug, Έβρος → Maritsa,
  Αξιός → Vardar, Жайық → Ural), or a Japanese name.

### The new naming's names

A Wikidata label that carries its description ("Lozuvatka River, tributary of Sea of Azov", "Malani (river)")
enters the table as the name alone ("Lozuvatka", "Malani"): 16 names, 95 values.

## Files

- `S2_provenance.tsv`: for every row and component, where the value came from (`S2`, the existing table;
  `S2-network`, the existing table's name for the river network; `S2-latin-part`; `S2-repaired`; `new`;
  `own-river`, the basin's own river as the system of a one-basin network; `new-same-river`;
  `S2-transliterated`; `S2-no-reading`, left blank; `new-repaired`).
- `S2_nonlatin_rivers.tsv`: every river name with no Latin letter, with its result, rule and evidence.
- `S2_review.tsv`: the 583 rows among the basins cheCkOVER 1.0 uses where the existing table and the new
  naming both name a river in Latin letters and the rivers differ. The existing value was kept in all of
  them. Each row gives the id, both sets of components, and the taxa that use the basin.
- `S2_mainbas_exceptions.tsv`: the river networks (MAIN_BAS) that carry more than one `Basin_name`. All 1,201
  of them were so in the existing table and are left as they were; the completion adds none.

Checks:
- (a) Every existing real value is unchanged in its component, apart from the Latin form, the look-alike
  letters, the punctuation of other scripts and the NFC / "?" repairs: 0 exceptions.
- (b) No river network gains a second `Basin_name`.
- No value holds a character of a script other than Latin (Common and Inherited aside), a character in
  U+3000–303F or U+FF00–FFEF, a "?", a carriage return, a double space, or a non-NFC form.

## How the new names were made

1. **Rivers.** HydroRIVERS v1.0 (Lehner & Grill 2013; 8,477,883 reaches), which carries for every reach the
   reach it flows into, its upstream area and its level-12 HydroBASINS unit. At every confluence the branch
   with the larger upstream area is the main stem; a river is followed from where it joins a larger one (or
   the sea) up its main stem.
2. **Names.** Wikidata watercourses (items with "mouth of the watercourse", P403; 159,759 items; CC0), with
   their English label, coordinates, drainage area (P2053), length (P2043) and GeoNames id; and GeoNames
   streams (feature codes STM, STMI; 1,253,287 points; CC-BY 4.0), in English where GeoNames or Wikidata
   gives an English form. Romanian ş/ţ are written ș/ț.
3. **Placing a name.** A point names the reach it lies on (within 3 km), or the reach that ends where it
   lies (within 1.5 km: a river's mouth). A name is placed only on a river of its own size: a river with a
   known drainage area only on a river draining between a quarter of that area and two and a half times it;
   with a known length, the same on length; with no size, only on rivers draining less than 10,000 km².
   A tributary that Wikidata says flows into the river it lies on is only that river's mouth there. A point
   at a confluence goes to the branch whose upstream area is nearest to its river's drainage area. A river
   of 5,000 km² or more whose points fall on small channels (a delta) is placed on the nearest reach of its
   own size within 25 km.
4. **One river, one name.** Spellings of one river (one Wikidata item, or names that only ever occur on it)
   are one river; a river is cut in two only at a real fork (the joining branch at least 30% of the main
   stem) or between two named rivers of very different size. A trailing "River" is dropped ("Olt"), unless
   what is left is an ordinary English word ("Red River").
5. **A basin's names.** Its river is the reach with the largest upstream area inside it; its tributary is the
   river that joins the system; its system is the river at the outlet.

## Sources to cite

- Lehner, B., Grill, G. (2013). Global river hydrography and network routing: baseline data and new
  approaches to study the world's large river systems. Hydrological Processes 27: 2171–2186.
  (HydroBASINS v1c; HydroRIVERS v1.0)
- Wikidata (CC0), queried 27 September 2026 (watercourses; their labels and aliases in every language).
- GeoNames (CC-BY 4.0), geonames.org, downloaded 27 September 2026 (streams, wadis, alternate names).
- ICU 77.1 transliterators (Unicode CLDR): BGN/PCGN, Han-Latin, Any-Latin.
- Eurostat GISCO country boundaries (for the country of a basin).

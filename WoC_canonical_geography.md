# WoC canonical geographic vocabulary

Derived from the full WoC export (124,255 records, 2026-07-24).
This is the authoritative vocabulary. Both WoC ingestion and cheCkOVER must use it.

## Continent — exactly 6 permitted values

| Continent | Records |
|---|---|
| North America | 60,150 |
| Europe | 56,733 |
| Asia | 2,830 |
| Oceania | 2,697 |
| South America | 1,038 |
| Africa | 763 |

Rules:
- No other value is permitted. `Australia` is NEVER a continent — use `Oceania`.
- Antarctica is not in use.
- Blank / NULL / empty string are NOT permitted.

## Country — 99 values in use

Style: Natural Earth English short names.
Canonical forms in use (do not substitute variants):

| Use this | NOT this |
|---|---|
| United States | USA, U.S.A., United States of America |
| United Kingdom | UK, Great Britain, England |
| Czech Republic | Czechia |
| Russia | Russian Federation |
| South Korea | Republic of Korea, Korea |
| Turkey | Türkiye |
| North Macedonia | Macedonia, FYROM |
| Eswatini | Swaziland |

Full country to continent mapping: see `WoC_canonical_country_continent.tsv`.

## Transcontinental countries — DO NOT derive continent from country alone

These 6 countries legitimately span more than one continent. The split is correct,
not an error. Continent must be resolved from coordinates for these:

| Country | Continents in use | Reason |
|---|---|---|
| France | Europe / North America / Africa / South America | Guadeloupe, Martinique, Reunion, French Guiana |
| United States | North America / Oceania | Hawaii is Oceania |
| Russia | Europe / Asia | transcontinental |
| Turkey | Asia / Europe | Thrace is Europe |
| Spain | Europe / Africa | Canary Islands, Ceuta, Melilla |
| Papua New Guinea | Oceania / Asia | 2 Asia records look erroneous - verify |

For all other countries, country to continent derivation is safe and deterministic.

## Known inconsistencies to correct

1. **69 records where county repeats the country name** (Denmark 51, Luxembourg 11,
   Mexico 7). Admin-1 should carry a real subnational unit, not the country name.
2. **Papua New Guinea, 2 records assigned to Asia** - PNG is Oceania; verify coordinates.
3. **44 records with blank continent / country / county** - see the integrity task.
4. **Fiji** appears in the country list (99 countries) but its single record has a
   blank continent; canonical continent is Oceania. Included in the TSV manually.

## Requirement

cheCkOVER must emit exactly these strings when it writes or reports geography.
Any new value must be added to this canon deliberately, never introduced ad hoc.
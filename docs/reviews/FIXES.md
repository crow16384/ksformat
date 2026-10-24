# Фиксы по находкам ревью 2026-10-24

## №2 — запятые в ключах ломают numeric-range (Med) — FIXED (d438e30)

Двухслойный фикс:
- `detect_format_type()` (R/format_create.R): ключ с запятой теперь помечается
  numeric ТОЛЬКО если реально парсится канонической range-грамматикой
  (`.parse_range_key`), либо если это ISO date-range (сохранён громкий
  legacy-путь, подталкивающий `type="date_range"`). Чистые текстовые ключи
  вроде `"San Francisco, CA"` остаются `character` при `type="auto"`.
- `.build_range_table()` (R/utilities.R): каждый распарсенный range валидируется
  (`low > high` или NA-границы → вырожденный). Вырожденный ключ деградирует в
  discrete с явным `cli_warn`, а не уходит в отсортированную fast-path-таблицу
  и не роняет `findInterval()` при fput.

Было: `fnew("1,000"="thousand")` → type numeric → `fput(1500)` → ERROR 'vec должен быть
отсортирован'. Стало: warning при создании ("parses as a range with low > high;
treating as discrete"), `fput(1500)` → `"1500"` (fallthrough original), без краха.
Текстовый `fput("1,000")` по-прежнему → `"thousand"`.

+5 тестов (comma auto-type, degenerate→discrete, mixed valid+degenerate,
reverse date_range, legacy date-auto path). Suite: 796 pass / 0 fail.

## №1 — приоритет пересекающихся диапазонов (High) — см. коммит 4868d09

## №3–№9 — FIXED (один коммит, suite 824/0)

- №3 POSIXct value-type ranges: bounds теперь парсятся .parse_datetime_range_key
  (UTC), вход сохраняет класс вместо строкового round-trip (латентный tz-drift
  тоже закрыт); обратные границы деградируют как в numeric.
- №4 .fput_vectorized: varning при NA в векторе имён форматов (результат тот же NA).
- №5 %OS: helper .strftime_with_os (POSIXct/datetime ветки) + "%05.2f" pad в numeric
  ветке; TIME11/TOD11/DATETIME22 сохраняют дробную часть.
- №6 .parse_num_pattern: overflow width/digits -> cli_abort на создании.
- №7 проверка дублей ключей для ignore_case форматов и инvalue делает
  tolower-коллизии видимыми (первый ключ достижим, второй — никогда).
- №8 fputk/finputk: общая валидация длин компонентов (.check_key_lengths).
- №9 .assert_no_empty_keys: fnew/finput/fparse (VALUE/INVALUE/vtype-блоки)
  отвергают ""-ключи с подсказкой про .missing (раньше fnew падал
  base-сообщением, fparse принимал недостижимое правило; в R-source `""=`
  вообще синтаксическая ошибка — отсюда «cryptic»).

## Виньетка (owner directive)

nonstandard-applications.qmd: новый подраздел "Overlapping ranges: declaration
order wins" (working example: narrow-first vs wide-first, multilabel+fput_all)
+ примечание про ключи с запятыми. Выводы чанков проверены прогоном.

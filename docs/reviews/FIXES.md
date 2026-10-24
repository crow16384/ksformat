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

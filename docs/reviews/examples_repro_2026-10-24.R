# ksformat 0.8.5 — репродукции подтверждённых находок ревью 2026-10-24
# Прогон: Rscript --vanilla examples_repro_2026-10-24.R
# Семантика №1 в этом билете НЕ показана (она в репо, ждёт сборку 0.8.6).
suppressMessages(library(ksformat))

B <- function(n, t) cat("\n=========================================================\nBUG", n, "-", t, "\n=========================================================\n")
run <- function(lbl, expr) {
  out <- withCallingHandlers(
    tryCatch(paste(format(as.vector(eval(parse(text = expr)))), collapse = " | "),
             error = function(e) paste0("ERROR: ", conditionMessage(e))),
    warning = function(w) { cat(sprintf("  warning> %s\n", gsub("\n", " ", conditionMessage(w)))); invokeRestart("muffleWarning") })
  cat(sprintf("  %-34s => %s\n", lbl, out))
}

B(2, "Ключи с запятой: авто-тип numeric -> крах findInterval (Med)")
cat("# '1,000' как ключ — это НЕ валидный range-ключ, но авто-детект видит запятую\n")
run("fnew('1,000'='thousand')", "fnew(\"1,000\" = \"thousand\", \"2,000\" = \"twoK\", name = \"thou\")$type")
cat("# тип 'numeric'; '1,000' распарсился как диапазон low=1, high=0 (!),\n# low<=high не валидируется; disjointness-проверка о вырожденных [1,0],[2,0]\n# проходит (0 > 2 ложно) -> sorted_disjoint=TRUE -> fast-path -> findInterval крах:\n")
rt <- format_get("thou")$range_table
run("rt$low", "rt$low"); run("rt$high", "rt$high")
run("rt$sorted_disjoint (проверка vacuous!)", "rt$sorted_disjoint")
cat("# текстовый поиск работает (дискретка):\n")
run("fput('1,000','thou')", "fput('1,000', 'thou')")
cat("# а числовой вход падает на apply:\n")
run("fput(1500, 'thou')", "fput(1500, 'thou')")
cat("# вариант (b): ключи-строки с запятой (адреса)\n")
run("fnew('San Francisco, CA'=...) type", "fnew(\"San Francisco, CA\" = \"SF\", name = \"city\")$type")
fclear()

B(3, "POSIXct value-type + range-ключи сломаны (Med)")
fnew("2024-06-01,2024-06-30,TRUE,TRUE" = as.POSIXct("2024-06-15 12:00", tz = "UTC"),
     type = "POSIXct", name = "vpc")
cat("# границы распарсились как Date, значения-POSIXct — сравнение рассинхронизировано:\n")
Sys.setenv(TZ = "UTC")
run("TZ=UTC  fput(2024-06-10 UTC)", "format(fput(as.POSIXct('2024-06-10 08:00', tz='UTC'), 'vpc'), usetz=FALSE)")
Sys.setenv(TZ = "America/New_York")
run("TZ=NY   fput(2024-06-10 UTC)", "format(fput(as.POSIXct('2024-06-10 08:00', tz='UTC'), 'vpc'), usetz=FALSE)")
Sys.setenv(TZ = "UTC")
cat("# datetime-ключ со временем в value-type СОЗДАЁТСЯ молча, но lookup ломается так же:\n")
run("fnew('2024-06-01 11:00:00,2024-06-01 13:00:00,TRUE,FALSE', type='POSIXct')",
    "fnew(\"2024-06-01 11:00:00,2024-06-01 13:00:00,TRUE,FALSE\" = as.POSIXct('2024-06-01 12:00', tz='UTC'), type='POSIXct', name='vpd')")
cat("# для сравнения: Date value-type + date-only ключи РАБОТАЮТ:\n")
fnew("2024-01-01,2025-01-01,TRUE,FALSE" = as.Date("2024-06-01"), type = "Date", name = "dtv")
run("fput(Date 2024-06, 'dtv')", "format(fput(as.Date('2024-06-15'), 'dtv'))")
fclear()

B(4, "fputn/fputc: NA в векторе имён форматов -> молча NA (Med-Low)")
fnew("0,10,TRUE,FALSE" = "lo", .missing = "MISSING", name = "f1")
cat("# формат знает про .missing, но позиция с NA-ИМЕНЕМ не доходит ни до чего:\n")
run("fputn(c(5,7,NA), c('f1','f1',NA))", "fputn(c(5,7,NA), c('f1', 'f1', NA))")
run("fputn(c(5,7),   c('f1', NA))",     "fputn(c(5,7), c('f1', NA))")
fclear()

B(5, "%OS: дробные секунды теряются на POSIXct / кривой вывод на numeric (Low)")
cat("# numeric-секунды: '12:30:0.50' — SAS дал бы 12:30:00.5;\n")
run("fputn(45000.5, 'TIME11.')", "fputn(45000.5, 'TIME11.')")
cat("# POSIXct: дробь молча ОТБРОШЕНА (0.75 -> '00:00:00'):\n")
run("fputn(POSIXct '00:00:00.75', 'TIME11.')", "fputn(as.POSIXct('2024-01-01 00:00:00.75', tz='UTC'), 'TIME11.')")
run("fputn(POSIXct '12:30:15.75', 'TIME11.')", "fputn(as.POSIXct('2024-01-01 12:30:15.75', tz='UTC'), 'TIME11.')")
fclear()

B(6, "Перелинт width в паттерне: создаётся, крашится на apply (Low)")
run("fnew('%10000000000.2f', type='numeric')", "fnew('%10000000000.2f', type = 'numeric', name = 'huge')$num_pattern_spec$width")
cat("# width=NA (as.integer перелинт, варнинги проглочены созданием);\n# apply — невнятный крах вместо ошибки валидации на fnew:\n")
run("fputn(3.14, 'huge')", "fputn(3.14, 'huge')")
fclear()

B(7, "finput(ignore_case=TRUE): nocase-дубли ключей молча, первый побеждает (Low)")
run("finput('Yes'=1, 'yes'=2, ignore_case=TRUE)", "finput(\"Yes\" = 1, \"yes\" = 2, name = 'yn', ignore_case = TRUE)$target_type")
cat("# точные дубли finput ловит, nocase-дубли — нет (см. warning ниже, если бы ловил);\n# теперь навсегда YES->1, 'yes'=2 недостижим:\n")
run("finputn('YES','yn')", "finputn('YES', 'yn')")
run("finputn('yes','yn')", "finputn('yes', 'yn')")
fclear()

B(8, "fputk/finputk: разная длина компонентов молча рециклится (Low)")
fnew("A|1" = "x", "A|2" = "y", "B|1" = "z", .missing = "MISS", name = "ck")
cat("# len 2 vs len 3: base-варнинг о рецикле, на выходе мусор-спарки:\n")
run("fputk(c('A','B'), c(1,2,NA))", "fputk(c('A','B'), c(1, 2, NA), format = 'ck')")
cat("# fput сам по себе такие несогласованные длины ловит — здесь проверки нет:\n")
run("fput(c(1,2), fmt, 1:3) для сравнения", "fput(c(1,2), 'ck', 1:3)")
fclear()

B(9, "''-ключи: fnew роняет невнятно, fparse принимает и делает недостижимым (Design)")
run("fnew('' = 'Blank')", "fnew(\"\" = \"Blank\", name = 'sh1')")
fparse(text = 'VALUE sh2 (character)\n  "" = "Blank"\n  .missing = "Unk"\n;')
cat("# fparse ключ '' принял; is_missing('') == TRUE => правило мёртво:\n")
run("fput('', 'sh2') (.missing выиграл)", "fput('', 'sh2')")
run("keep_na=TRUE", "fput('', 'sh2', keep_na = TRUE)")
cat("# без .missing тоже NA, не 'Blank':\n")
fclear("sh2")
fparse(text = 'VALUE sh3 (character)\n  "" = "Blank"\n;')
run("fput('', 'sh3') (ожидание 'Blank')", "fput('', 'sh3')")
fclear()

cat("\nDONE — все выводы выше сняты с установленного ksformat 0.8.5\n")
